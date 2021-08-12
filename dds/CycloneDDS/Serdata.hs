{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module CycloneDDS.Serdata
  (SerdataKind(..),
   Serdata,
   Keyhash(..),
   SerdataOps,
   Sertype,
   SertypeOps,
   SamplePtr(..),
   newSertype
  ) where

import CycloneDDS.Serdata.Raw

import Foreign.C
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
--import Foreign.CStorable
--import Control.Monad (when, void)
import Control.Concurrent.MVar

--import Data.Kind
import Data.Word
--import Data.Int
--import Data.Vector.Fixed.Unboxed as VFU

import qualified Data.ByteString as B;
import qualified Data.ByteString.Lazy as BL;
import Data.ByteString.Builder

import Data.Serialize

import System.Posix.Types.Iovec

import Data.Proxy

--type SdoFromSer a = Ptr (Sertype a) -> SerdataKind -> Ptr Fragchain -> CSize -> IO (Ptr (Serdata a))

{- 
  uint32_t off = 4; /* must skip the CDR header */

  assert (fragchain->min == 0);
  assert (fragchain->maxp1 >= off); /* CDR header must be in first fragment */

  memcpy (&d->hdr, NN_RMSG_PAYLOADOFF (fragchain->rmsg, NN_RDATA_PAYLOAD_OFF (fragchain)), sizeof (d->hdr));
  assert (d->hdr.identifier == CDR_LE || d->hdr.identifier == CDR_BE);

  while (fragchain)
  {
    assert (fragchain->min <= off);
    assert (fragchain->maxp1 <= size);
    if (fragchain->maxp1 > off)
    {
      /* only copy if this fragment adds data */
      const unsigned char *payload = NN_RMSG_PAYLOADOFF (fragchain->rmsg, NN_RDATA_PAYLOAD_OFF (fragchain));
      serdata_default_append_blob (&d, fragchain->maxp1 - off, payload + off - fragchain->min);
      off = fragchain->maxp1;
    }
    fragchain = fragchain->nextfrag;
  }
-}

setCDR :: SerdataContent a -> (Ptr Word8, CSize) -> IO (Ptr Word8, CSize)
setCDR c cdr = do
  res <- tryPutMVar (sdCDR c) cdr
  case res of
    True  -> pure cdr
    False -> free (fst cdr) >> readMVar (sdCDR c)

forceCDR :: (Serialize a, Show a) => SerdataKind -> SerdataContent a -> IO (Ptr Word8, CSize)
forceCDR Empty _ = undefined
forceCDR Key   c = do
  ptr <- callocBytes 4 :: IO (Ptr Word8) -- encoding = 0 (BE), options = 0
  setCDR c (ptr, 4)
forceCDR Data  c = do
  let a = case sdSample c of
            Just x -> x
            Nothing -> undefined -- FIXME: I only think it can't happen
  --putStrLn $ "makeCDR: serializing " ++ show a
  let bs = toLazyByteString (word16BE 0 `mappend` word16BE 0 `mappend` (byteString . encode) a)
      l = BL.unpack bs
  --putStrLn $ "makeCDR: blob " ++ show l
  ptr <- newArray l
  setCDR c (ptr, fromIntegral $ length l)

readCDR :: (Serialize a, Show a) => SerdataKind -> SerdataContent a -> IO (Ptr Word8, CSize)
readCDR kind c = do
  cdr <- tryReadMVar (sdCDR c)
  case cdr of
    Just x -> pure x
    Nothing -> forceCDR kind c

dropCDR :: SerdataContent a -> IO ()
dropCDR c = tryTakeMVar (sdCDR c) >>= \cdr -> case cdr of
  Nothing -> pure ()
  Just (ptr, _) -> free ptr

serdataFromSerCommon :: (Serialize a, Show a) => Ptr (Sertype a) -> SerdataKindInt -> BL.ByteString -> IO (Ptr (Serdata a))
serdataFromSerCommon sertypeptr kind bs = do
  --putStrLn "serdataFromSerCommon begin"
  case decodeLazy bs of
    Left _ {-str-} -> do
      --putStrLn $ "serdataFromSerCommon error: " ++ str
      return nullPtr
    Right val -> do
      sdptr <- calloc -- malloc, zero, init, read, modify, write is not smart! 'twould be better to just poke
      c_ddsi_serdata_init sdptr sertypeptr kind
      sd <- peek sdptr
      hash <- stSerdataBasehash <$> peek sertypeptr -- keyless for now
      cdr <- newEmptyMVar
      contentptr <- SerdataContentPtr <$> newStablePtr SerdataContent { sdSample = Just val, sdCDR = cdr }
      poke sdptr $ sd { sdIoxChunk = nullPtr, sdIoxSubscriber = nullPtr, sdHash = hash, sdContent = contentptr }
      --putStrLn "serdataFromSerCommon end"
      return sdptr

bytestringFromFragchain :: Ptr Fragchain -> IO BL.ByteString
bytestringFromFragchain fcptr0 = do
  -- 4 bytes skips the encoding header, we know it's there, though we should check
  -- just in case
  go mempty 4 fcptr0
  where
    go :: Builder -> Word32 -> Ptr Fragchain -> IO BL.ByteString
    go !accum !off !fcptr
      | fcptr == nullPtr = return $ toLazyByteString accum
      | otherwise = do
          -- C code still checks whether fragment adds data, but underlying implementation
          -- actually guarantees that, so no real need for that here
          fc <- peek fcptr
          let n = fcMaxp1 fc - off
          let payloadptr = fragchainPayload fc
          -- I think: unsafePackCStringFinalizer :: Ptr Word8 -> Int -> IO () -> IO ByteString
          -- and then do a strict deserialization would make the most sense, but for now,
          -- play it safe and make a copy
          let cslen = (castPtr payloadptr `plusPtr` fromIntegral off, fromIntegral n)
          chunk <- B.packCStringLen cslen
          go (accum `mappend` byteString chunk) (off+n) (fcNextfrag fc)

bytestringFromSerIovec :: CInt -> Ptr CIovec -> IO BL.ByteString
bytestringFromSerIovec niov0 iovptr0 = do -- just like bytestringFromFragchain
  go mempty 4 niov0 iovptr0
  where
    go !accum _ 0 _ = return $ toLazyByteString accum
    go !accum !off !niov !iovptr = do
      iov <- peek iovptr
      let n = iov_len iov - off
      let cslen = (castPtr (iov_base iov) `plusPtr` fromIntegral off, fromIntegral n)
      chunk <- B.packCStringLen cslen
      go (accum `mappend` byteString chunk) 0 (niov-1) (iovptr `plusPtr` sizeOf (undefined :: CIovec))

serdataFromSer :: (Serialize a, Show a) => SdoFromSer a
serdataFromSer sertypeptr kind fcptr _ =
  bytestringFromFragchain fcptr >>= serdataFromSerCommon sertypeptr kind

serdataFromSerIovec :: (Serialize a, Show a) => SdoFromSerIovec a
serdataFromSerIovec sertypeptr kind niov iov _ = do
  bytestringFromSerIovec niov iov >>= serdataFromSerCommon sertypeptr kind

serdataFromSample :: (Serialize a, Show a) => SdoFromSample a
serdataFromSample stptr kind samplePtr = do
  --putStrLn "serdataFromSample begin"
  sample <- deRefStablePtr $ unSamplePtr samplePtr
  sdptr <- calloc -- FIXME: there are better ways dan malloc/memset/read/modify/write
  c_ddsi_serdata_init sdptr stptr kind
  sd <- peek sdptr
  hash <- stSerdataBasehash <$> peek stptr -- keyless for now
  cdr <- newEmptyMVar
  let content = case unmarshalSerdataKind kind of
        Empty -> undefined
        Key -> SerdataContent { sdSample = Nothing, sdCDR = cdr }
        Data -> SerdataContent { sdSample = Just sample, sdCDR = cdr }
  contentptr <- SerdataContentPtr <$> newStablePtr content
  poke sdptr $ sd { sdHash = hash, sdContent = contentptr }
  --putStrLn "serdataFromSample end"
  pure sdptr

serdataContent :: Serdata a -> IO (SerdataContent a)
serdataContent sd = deRefStablePtr $ unSerdataContentPtr $ sdContent sd

serdataToSer :: (Serialize a, Show a) => SdoToSer a
serdataToSer sdptr offset size bufptr = do
  sd <- peek sdptr
  content <- serdataContent sd
  (cdr, _) <- readCDR (sdKind sd) content
  copyArray bufptr (cdr `plusPtr` fromIntegral offset) (fromIntegral size)

serdataToSerRef :: (Serialize a, Show a) => SdoToSerRef a
serdataToSerRef sdptr offset size iovecptr = do
  sd <- peek sdptr
  content <- serdataContent sd
  (cdr, _) <- readCDR (sdKind sd) content
  poke iovecptr $ CIovec { iov_base = cdr `plusPtr` fromIntegral offset, iov_len = size }
  c_ddsi_serdata_ref sdptr

serdataToSerUnref :: SdoToSerUnref a
serdataToSerUnref sdptr _ = c_ddsi_serdata_unref sdptr

serdataGetSize :: (Serialize a, Show a) => SdoGetSize a
serdataGetSize sdptr = do
  --putStrLn $ "serdataGetSize begin"
  sd <- peek sdptr
  content <- serdataContent sd
  (_, size) <- readCDR (sdKind sd) content
  --putStrLn $ "serdataGetSize: " ++ show size
  pure $ fromIntegral size

serdataToUntyped :: SdoToUntyped a
serdataToUntyped sdptr = do
  sd <- peek sdptr
  sdptr' <- calloc -- FIXME: there are better ways dan malloc/memset/read/modify/write
  c_ddsi_serdata_init_untyped sdptr' (sdType sd) (marshalSerdataKind Key)
  sd' <- peek sdptr'
  let hash = sdHash sd
  -- FIXME: yucky hack of clearing sdType seems to not want to go away
  -- (sdType is meaningless if untyped; ddsi_serdata_init doesn't like a null pointer)
  -- keyless, so no need to worry much about SerdataContent
  cdr <- newEmptyMVar
  contentptr' <- SerdataContentPtr <$> (newStablePtr $ SerdataContent { sdSample = Nothing, sdCDR = cdr }) :: IO (SerdataContentPtr ())
  poke sdptr' $ sd' { sdHash = hash, sdType = nullPtr, sdContent = contentptr' }
  return sdptr'

serdataFree :: SdoFree a
serdataFree sdptr = do
  sd <- peek sdptr
  content <- serdataContent sd
  dropCDR content
  freeStablePtr $ unSerdataContentPtr $ sdContent sd
  free sdptr

serdataPrint :: Show a => SdoPrint a
serdataPrint _ sdptr buf size
  | size <= 0 = pure 0
  | otherwise = do
      sd <- peek sdptr
      content <- serdataContent sd
      let text = maybe "(no sample)" show (sdSample content)
          tocopy = take (fromIntegral (size-1)) text
          asbytes = map castCharToCChar tocopy
      pokeArray0 0 buf asbytes
      pure $ fromIntegral $ length tocopy

sertypeFree :: StoFree a
sertypeFree stptr = do
  st <- peek stptr
  freeSertypeOpsPtr $ stOps st
  freeSerdataOpsPtr $ stSerdataOps st
  free stptr

newSertype :: (Serialize a, Show a) => Proxy a -> String -> IO (Ptr (Sertype a))
newSertype _ name = do
  -- Baking new sertype, serdata ops means we will never get different types in
  -- serdataEqual, which is easy and unfortunate at the same time: easy because it means
  -- less to worry about, unfortunate because it means all topics with use unique instance
  -- handles.  Oh well.
  stops <- newSertypeOpsPtr $ SertypeOps
        { stoFree = sertypeFree
        , stoEqual = \_ _ -> True
        , stoHash = \_ -> 0
        , stoZeroSamples = undefined
        , stoReallocSamples = undefined
        , stoFreeSamples = undefined
        }
  sdops <- newSerdataOpsPtr $ SerdataOps
        { sdoFromSer = serdataFromSer
        , sdoFromSerIovec = serdataFromSerIovec
        , sdoEqkey = \_ _ -> True -- FIXME: this is only correct for keyless topics
        , sdoFromSample = serdataFromSample
        , sdoToSerRef = serdataToSerRef
        , sdoToSerUnref = serdataToSerUnref
        , sdoGetSize = serdataGetSize
        , sdoToUntyped = serdataToUntyped
        , sdoFree = serdataFree
        , sdoPrint = serdataPrint
        , sdoFromKeyhash = undefined
        , sdoToSer = serdataToSer
        , sdoToSample = undefined
        , sdoUntypedToSample = undefined
        , sdoGetKeyhash = undefined
        }
  stptr <- calloc
  let flags = sertypeTopicKindNoKey -- FIXME
  withCString name $ \cname -> c_ddsi_sertype_init_flags stptr cname stops sdops flags
  pure stptr
