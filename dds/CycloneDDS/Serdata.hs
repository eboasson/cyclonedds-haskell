{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module CycloneDDS.Serdata
  (SerdataKind(..),
   Serdata,
   Keyhash(..),
   SerdataOps,
   Sertype,
   SertypeOps,
   SamplePtr(..),
   newSertype,
   -- dirty A
   SamplesBlob,
   -- dirty B
   SerdataContent(..),
   serdataContent,
   c_ddsi_serdata_unref,
   TopicType(..)
  ) where

import CycloneDDS.Serdata.Raw
import CycloneDDS.Sample

import Foreign.C
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Control.Concurrent.MVar

import Data.Word

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder
import Data.IORef
import Data.Bits
import System.Posix.Types.Iovec

import Data.Proxy

setCDR :: SerdataContent a -> (Ptr Word8, CSize) -> IO (Ptr Word8, CSize)
setCDR content cdr = do
  res <- tryPutMVar (sdCDR content) cdr
  case res of
    True  -> pure cdr
    False -> free (fst cdr) >> readMVar (sdCDR content)

forceCDR :: TopicType a => SerdataKind -> SerdataContent a -> IO (Ptr Word8, CSize)
forceCDR kind content = do
  --putStrLn $ "makeCDR: serializing " ++ show a
  let bs = case (kind, sdSample content) of
        (SerdataKindEmpty, _)       -> undefined
        (SerdataKindKey, Key z)     -> encodeKey z
        (SerdataKindKey, Sample z)  -> encodeKey $ getKey z
        (SerdataKindData, Key _)    -> undefined
        (SerdataKindData, Sample z) -> encodeSample z
      l = BL.unpack bs
  --putStrLn $ "makeCDR: blob " ++ show l
  ptr <- newArray l
  setCDR content (ptr, fromIntegral $ length l)

readCDR :: TopicType a => SerdataKind -> SerdataContent a -> IO (Ptr Word8, CSize)
readCDR kind c = do
  cdr <- tryReadMVar (sdCDR c)
  case cdr of
    Just x -> pure x
    Nothing -> forceCDR kind c

dropCDR :: SerdataContent a -> IO ()
dropCDR c = tryTakeMVar (sdCDR c) >>= \cdr -> case cdr of
  Nothing -> pure ()
  Just (ptr, _) -> free ptr

serdataFromSerCommon :: TopicType a => Ptr (Sertype a) -> SerdataKindInt -> BL.ByteString -> IO (Ptr (Serdata a))
serdataFromSerCommon sertypeptr kind bs = do
  --putStrLn "serdataFromSerCommon begin"
  case decodeSample bs of
    Left _ {-str-} -> do
      --putStrLn $ "serdataFromSerCommon error: " ++ str
      return nullPtr
    Right val -> do
      sdptr <- calloc -- malloc, zero, init, read, modify, write is not smart! 'twould be better to just poke
      c_ddsi_serdata_init sdptr sertypeptr kind
      sd <- peek sdptr
      hash <- stSerdataBasehash <$> peek sertypeptr -- keyless for now
      cdr <- newEmptyMVar
      contentptr <- SerdataContentPtr <$> newStablePtr SerdataContent { sdSample = Sample val, sdCDR = cdr }
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

serdataFromSer :: TopicType a => SdoFromSer a
serdataFromSer sertypeptr kind fcptr _ =
  bytestringFromFragchain fcptr >>= serdataFromSerCommon sertypeptr kind

serdataFromSerIovec :: TopicType a => SdoFromSerIovec a
serdataFromSerIovec sertypeptr kind niov iov _ = do
  bytestringFromSerIovec niov iov >>= serdataFromSerCommon sertypeptr kind

serdataFromSample :: TopicType a => SdoFromSample a
serdataFromSample stptr kind samplePtr = do
  --putStrLn "serdataFromSample begin"
  sample <- deRefStablePtr $ unSamplePtr samplePtr
  sdptr <- calloc -- FIXME: there are better ways dan malloc/memset/read/modify/write
  c_ddsi_serdata_init sdptr stptr kind
  sd <- peek sdptr
  hash <- stSerdataBasehash <$> peek stptr -- keyless for now
  cdr <- newEmptyMVar
  let content = case unmarshalSerdataKind kind of
        SerdataKindEmpty -> undefined
        SerdataKindKey -> SerdataContent { sdSample = Key (getKey sample), sdCDR = cdr }
        SerdataKindData -> SerdataContent { sdSample = Sample sample, sdCDR = cdr }
  contentptr <- SerdataContentPtr <$> newStablePtr content
  poke sdptr $ sd { sdHash = hash, sdContent = contentptr }
  --putStrLn "serdataFromSample end"
  pure sdptr

serdataContent :: Serdata a -> IO (SerdataContent a)
serdataContent sd = deRefStablePtr $ unSerdataContentPtr $ sdContent sd

serdataToSer :: TopicType a => SdoToSer a
serdataToSer sdptr offset size bufptr = do
  sd <- peek sdptr
  content <- serdataContent sd
  (cdr, _) <- readCDR (sdKind sd) content
  copyArray bufptr (cdr `plusPtr` fromIntegral offset) (fromIntegral size)

serdataToSerRef :: TopicType a => SdoToSerRef a
serdataToSerRef sdptr offset size iovecptr = do
  sd <- peek sdptr
  content <- serdataContent sd
  (cdr, _) <- readCDR (sdKind sd) content
  poke iovecptr $ CIovec { iov_base = cdr `plusPtr` fromIntegral offset, iov_len = size }
  c_ddsi_serdata_ref sdptr

serdataToSerUnref :: SdoToSerUnref a
serdataToSerUnref sdptr _ = c_ddsi_serdata_unref sdptr

serdataGetSize :: TopicType a => SdoGetSize a
serdataGetSize sdptr = do
  --putStrLn $ "serdataGetSize begin"
  sd <- peek sdptr
  content <- serdataContent sd
  (_, size) <- readCDR (sdKind sd) content
  --putStrLn $ "serdataGetSize: " ++ show size
  pure $ fromIntegral size

serdataToUntyped :: TopicType a => SdoToUntyped a
serdataToUntyped sdptr = do
  sd <- peek sdptr
  content <- serdataContent sd
  sdptr' <- calloc -- FIXME: there are better ways dan malloc/memset/read/modify/write
  c_ddsi_serdata_init sdptr' (sdType sd) (marshalSerdataKind SerdataKindKey)
  sd' <- peek sdptr'
  let hash = sdHash sd
  -- FIXME: yucky hack of clearing sdType seems to not want to go away
  -- (sdType is meaningless if untyped; ddsi_serdata_init doesn't like a null pointer)
  -- keyless, so no need to worry much about SerdataContent
  cdr <- newEmptyMVar
  let sample' = case sdSample content of
                  (Key z) -> Key z
                  (Sample z) -> Key $ getKey z
      content' = SerdataContent { sdSample = sample', sdCDR = cdr }
  contentptr' <- SerdataContentPtr <$> (newStablePtr content') -- :: IO (SerdataContentPtr ())
  poke sdptr' $ sd' { sdHash = hash, sdType = nullPtr, sdContent = contentptr' }
  return sdptr'

-- Very hacky: sampleptr is one of the elements in the "ptrs" array filled by
-- "sertypeReallocSamples", and so a
--   (Ptr (SamplesBlob a)) = (Ptr (StablePtr (IORef [Sample a])))
-- and we simply prepend to that list.  Methinks I can get away with this because of the
-- circumstances under which serdataToSamples is used.
serdataToSample :: SdoToSample a
serdataToSample sdptr sampleptr bufp buflim
  | bufp /= nullPtr = undefined
  | buflim /= nullPtr = undefined
  | otherwise = do
      sd <- peek sdptr
      content <- serdataContent sd
      let sample = sdSample content
      --putStrLn $ "serdataToSample: sampleptr " ++ show sampleptr
      iorefptr <- peek sampleptr
      --putStrLn $ "serdataToSample: iorefptr " ++ show iorefptr
      ioref <- deRefStablePtr iorefptr
      modifyIORef' ioref (sample :)
      pure True

serdataFree :: SdoFree a
serdataFree sdptr = do
  sd <- peek sdptr
  content <- serdataContent sd
  dropCDR content
  freeStablePtr $ unSerdataContentPtr $ sdContent sd
  free sdptr

serdataPrint :: (TopicType a, Show a) => SdoPrint a
serdataPrint _ sdptr buf size
  | size <= 0 = pure 0
  | otherwise = do
      sd <- peek sdptr
      content <- serdataContent sd
      let text = case sdSample content of
                   Key z -> show z
                   Sample z -> show z
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

-- ptrs is void ** and really expected to be so (but what ptrs[i] actually points to is a don't care for Cyclone)
-- old may be null, else is the address of a previously allocated blob for `oldcount` elements
-- on return ptrs[0] must point to the (possibly reallocated) blob
-- all ptrs[0<i<count] must point to something meaningful for ToSample, FreeSamples
-- the type of blob is really up to the implementation
-- it only ever uses ptrs starting from index 0 and incrementing the index
-- 
-- blob: (StablePtr (IORef [a]))
sertypeReallocSamples :: StoReallocSamples a
sertypeReallocSamples ptrs _st old oldcount count
  | count == 0       = undefined
  | count < oldcount = undefined
  | old == nullPtr   = newblob >>= rebuildPtrs
  | otherwise        = rebuildPtrs old
  where
    newblob = do
      blob <- malloc
      --putStrLn $ "sertypeReallocSamples: blob " ++ show blob
      ioref <- newIORef []
      iorefptr <- newStablePtr ioref
      --putStrLn $ "sertypeReallocSamples: iorefptr " ++ show iorefptr
      poke blob iorefptr
      pure blob
    rebuildPtrs blob = pokeArray ptrs (replicate (fromIntegral count) blob)

sertypeZeroSamples :: StoZeroSamples a
sertypeZeroSamples _st blob _count = do
  --putStrLn $ "sertypeZeroSamples: blob " ++ show blob
  iorefptr <- peek blob
  --putStrLn $ "sertypeZeroSamples: iorefptr " ++ show iorefptr
  ioref <- deRefStablePtr iorefptr
  writeIORef ioref []

sertypeFreeSamples :: StoFreeSamples a
sertypeFreeSamples st ptrs count flags
  | (flags .&. 4) == 0 = do
      --putStrLn $ "sertypeFreeSamples (content only): ptrs " ++ show ptrs
      blob <- peek ptrs
      --putStrLn $ "sertypeFreeSamples (content only): blob " ++ show blob
      sertypeZeroSamples st blob count
  | otherwise = do
      putStrLn $ "sertypeFreeSamples (content only): ptrs " ++ show ptrs
      blob <- peek ptrs
      --putStrLn $ "sertypeFreeSamples (all): blob " ++ show blob
      iorefptr <- peek blob
      --putStrLn $ "sertypeFreeSamples (all): blob " ++ show iorefptr
      freeStablePtr iorefptr
      free blob

{-
instance Show (StablePtr a) where
  show = show . ptrToWordPtr . castStablePtrToPtr
-}

newSertype :: TopicType a => Proxy a -> String -> IO (Ptr (Sertype a))
newSertype proxy name = do
  -- Baking new sertype, serdata ops means we will never get different types in
  -- serdataEqual, which is easy and unfortunate at the same time: easy because it means
  -- less to worry about, unfortunate because it means all topics with use unique instance
  -- handles.  Oh well.
  stops <- newSertypeOpsPtr $ SertypeOps
        { stoFree = sertypeFree
        , stoEqual = \_ _ -> True
        , stoHash = \_ -> 0
        , stoZeroSamples = sertypeZeroSamples
        , stoReallocSamples = sertypeReallocSamples
        , stoFreeSamples = sertypeFreeSamples
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
        , sdoToSample = serdataToSample
        , sdoUntypedToSample = undefined
        , sdoGetKeyhash = undefined
        }
  stptr <- calloc
  let flags = if hasKey proxy then 0 else sertypeTopicKindNoKey
  withCString name $ \cname -> c_ddsi_sertype_init_flags stptr cname stops sdops flags
  pure stptr
