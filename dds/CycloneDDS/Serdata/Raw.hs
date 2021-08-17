{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
    
#include "dds/features.h"
-- see dds/ddsi/ddsi_serdata.h for the gory details on the C side

module CycloneDDS.Serdata.Raw
  (SerdataKind(..),
   SerdataKindInt(..),
   marshalSerdataKind,
   unmarshalSerdataKind,
   Serdata(..),
   SerdataContent(..),
   SamplePtr(..),
   SerdataContentPtr(..),
   SamplesBlob,
   Keyhash(..),
   SerdataOps(..),
   SerdataOpsPtr,
   SdoEqkey, SdoGetSize, SdoFromSer, SdoFromSerIovec,
   SdoFromKeyhash, SdoFromSample, SdoToSer, SdoToSerRef,
   SdoToSerUnref, SdoToSample, SdoToUntyped, SdoUntypedToSample,
   SdoFree, SdoPrint, SdoGetKeyhash, SdoGetSampleSize, SdoFromIoxBuffer,
   Sertype(..),
   SertypeOps(..),
   SertypeOpsPtr,
   StoFree, StoZeroSamples, StoReallocSamples, StoFreeSamples,
   StoEqual, StoHash, StoTypeidHash, StoSerializedSize,
   StoSerialize, StoDeserialize, StoAssignableFrom,
   Fragchain(..),
   fragchainPayload,
   newSertypeOpsPtr, freeSertypeOpsPtr,
   newSerdataOpsPtr, freeSerdataOpsPtr,
   SertypeInitFlags(..),
   sertypeTopicKindNoKey, sertypeRequestKeyhash, sertypeFixedSize,
   c_ddsi_sertype_init_flags,
   c_ddsi_serdata_init,
   c_ddsi_serdata_ref, c_ddsi_serdata_unref
  ) where

import GHC.Generics (Generic)
import Foreign.C
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.CStorable

import Data.Word
import Data.Int
import Data.IORef
import Control.Concurrent.MVar
import Data.Vector.Fixed.Unboxed as VFU
import System.Posix.Types.Iovec

import CycloneDDS.Sample

data SerdataKind = SerdataKindEmpty | SerdataKindKey | SerdataKindData deriving (Show, Eq, Enum)

newtype SerdataKindInt = SerdataKindInt { unSerdataKindInt :: CInt } deriving (Storable)
marshalSerdataKind :: SerdataKind -> SerdataKindInt
marshalSerdataKind = SerdataKindInt . fromIntegral . fromEnum
unmarshalSerdataKind :: SerdataKindInt -> SerdataKind
unmarshalSerdataKind = toEnum . fromIntegral . unSerdataKindInt

-- It seem CStorable doesn't like Enums, but it also seems that defining
-- an instance explicitly is ok
instance CStorable SerdataKind where
  cSizeOf _ = sizeOf (undefined :: CInt)
  cAlignment _ = alignment (undefined :: CInt)
  cPeek ptr = toEnum . fromIntegral <$> peek (castPtr ptr :: Ptr CInt)
  cPoke ptr = poke (castPtr ptr :: Ptr CInt) . fromIntegral . fromEnum

-- newtype was "suggested" by GHC :)
newtype SamplePtr a = SamplePtr { unSamplePtr :: StablePtr a } deriving (Storable)
instance CStorable (SamplePtr a) where
  cSizeOf = sizeOf
  cAlignment = alignment
  cPeek = peek
  cPoke = poke
newtype SerdataContentPtr a = SerdataContentPtr { unSerdataContentPtr :: StablePtr (SerdataContent a) } deriving (Storable)
instance CStorable (SerdataContentPtr a) where
  cSizeOf = sizeOf
  cAlignment = alignment
  cPeek = peek
  cPoke = poke

data SerdataContent a = SerdataContent
  { sdSample :: Sample a -- use MVar as well? (currently it set only once)
  , sdCDR :: MVar (Ptr Word8, CSize) -- ByteArray?
  }

data Serdata a = Serdata
  { sdOps :: Ptr (SerdataOpsC a)
  , sdHash :: Word32
  , sdRefc :: Word32
  , sdKind :: SerdataKind
  , sdType :: Ptr (Sertype a)
  , sdTimestamp :: Word64
  , sdStatusinfo :: Word32
  , sdTWrite :: Word64
  -- always having an IoxChunk/IoxSubscriber that is not used is easier then having it
  -- only when CycloneDDS itself has it
  , sdIoxChunk :: Ptr ()
  , sdIoxSubscriber :: Ptr ()
  , sdContent :: SerdataContentPtr a
  } deriving (Generic)
instance CStorable (Serdata a)
instance Storable (Serdata a) where
  sizeOf = cSizeOf
  alignment = cAlignment
  peek = cPeek
  poke = cPoke

newtype Keyhash = Keyhash { unKeyhash :: VFU.Vec 16 Word8 }
  deriving (Show, Eq, Storable)

data GV
data Sertype a = Sertype -- not sure whether it is attractive to type this
  { stOps :: SertypeOpsPtr a
  , stSerdataOps :: SerdataOpsPtr a
  , stSerdataBasehash :: Word32
  , stOptionFlags :: Word32 -- bitfields, in C
  , stTypeName :: CString
  , stGV :: Ptr GV
  , stFlagsRefc :: Word32
  , stTimestamp :: Word64
  , stWrappedSertopic :: Ptr ()
  -- always having an IoxSize that is not used is easier then having it only when
  -- CycloneDDS itself has it
  , stIoxSize :: Word32
  } deriving (Generic)
instance CStorable (Sertype a)
instance Storable (Sertype a) where
  sizeOf = cSizeOf
  alignment = cAlignment
  peek = cPeek
  poke = cPoke

{-
struct nn_rmsg
  ddsrt_atomic_uint32_t refcount;
  struct nn_rmsg_chunk *lastchunk;
  bool trace;
  struct nn_rmsg_chunk chunk;
struct nn_rmsg_chunk
  struct nn_rbuf *rbuf;
  struct nn_rmsg_chunk *next;
  union {
    uint32_t size;
    int64_t l;
    double d;
    void *p;
  } u;
  /* unsigned char payload[] -- disallowed by C99 because of nesting */
-}
data RMsg
data RMsgChunk

-- on "normal" machines, the horrible offset calculations below should work out fine
rmsgChunkData :: Ptr RMsgChunk -> Ptr Word8
rmsgChunkData ptr = castPtr ptr `plusPtr` offset
  where
    offset = 2 * sizeOf (undefined :: Ptr ()) + a
    a = fromIntegral $ maximum [alignment (undefined::Int64), alignment (undefined::Ptr ()), alignment (undefined::CDouble)]
rmsgData :: Ptr RMsg -> Ptr Word8
rmsgData ptr = rmsgChunkData $ castPtr ptr `plusPtr` (4 * sizeOf (undefined :: Ptr ()))

data Fragchain = Fragchain -- nn_rdata in C, but exclusively used as a "fragchain" here
  { fcRMsg :: Ptr RMsg          -- pointer to enclosing RTPS message
  , fcNextfrag :: Ptr Fragchain -- next fragment in chain
  , fcMin :: Word32             -- first byte offset in fragment
  , fcMaxp1 :: Word32           -- offset of first byte after fragment
  , fcSubmsgZoff :: Word16      -- offset to submessage from packet start or 0
  , fcPayloadZoff :: Word16     -- offset to payload from packet start
  , fcKeyhashZoff :: Word16     -- offset to keyhash from packet start, or 0
  -- possibly some debug info, but we never allocate, store or use arrays of these, so we ignore it
  } deriving (Generic)
instance CStorable Fragchain
instance Storable Fragchain where
  sizeOf = cSizeOf
  alignment = cAlignment
  peek = cPeek
  poke = undefined
-- all that very low-level junk just to get this:
fragchainPayload :: Fragchain -> Ptr Word8
fragchainPayload fc = rmsgData (fcRMsg fc) `plusPtr` fromIntegral (fcPayloadZoff fc)

type SamplesBlob a = StablePtr (IORef [Sample a])

type SdoEqkey a = Ptr (Serdata a) -> Ptr (Serdata a) -> Bool
type SdoGetSize a = Ptr (Serdata a) -> IO Word32 -- pure, but lazy serialization means we'd have to do unsafeIO
type SdoFromSer a = Ptr (Sertype a) -> SerdataKindInt -> Ptr Fragchain -> CSize -> IO (Ptr (Serdata a))
type SdoFromSerIovec a = Ptr (Sertype a) -> SerdataKindInt -> CInt -> Ptr CIovec -> CSize -> IO (Ptr (Serdata a)) -- FIXME: ddsrt_msg_iovlen_t
type SdoFromKeyhash a = Ptr (Sertype a) -> Ptr Keyhash -> IO (Ptr (Serdata a))
type SdoFromSample a = Ptr (Sertype a) -> SerdataKindInt -> SamplePtr a -> IO (Ptr (Serdata a))
type SdoToSer a = Ptr (Serdata a) -> CSize -> CSize -> Ptr Word8 -> IO ()
type SdoToSerRef a = Ptr (Serdata a) -> CSize -> CSize -> Ptr CIovec -> IO (Ptr (Serdata a))
type SdoToSerUnref a = Ptr (Serdata a) -> Ptr CIovec -> IO ()
type SdoToSample a = Ptr (Serdata a) -> Ptr (SamplesBlob a) -> Ptr (Ptr Word8) -> Ptr Word8 -> IO Bool
type SdoToUntyped a = Ptr (Serdata a) -> IO (Ptr (Serdata a)) -- FIXME: Serdata (XKey a)?
type SdoUntypedToSample a = Ptr (Sertype a) -> Ptr (Serdata a) -> Ptr (SamplesBlob a) -> Ptr (Ptr Word8) -> Ptr Word8 -> IO Bool -- FIXME: Serdata (XKey a)
type SdoFree a = Ptr (Serdata a) -> IO ()
type SdoPrint a = Ptr (Sertype a) -> Ptr (Serdata a) -> Ptr CChar -> CSize -> IO CSize
type SdoGetKeyhash a = Ptr (Serdata a) -> Ptr Keyhash -> Bool -> IO ()
  -- we never use these and we set them nullPtrs
  -- we never have a need to peek in a SerdataOps
  -- if CycloneDDS was built without DDS_HAS_SHM it'll just be ignored completely
type SdoGetSampleSize a = Ptr (Serdata a) -> IO Word32
type SdoFromIoxBuffer a = Ptr (Sertype a) -> SerdataKindInt -> Ptr () -> Ptr ()

data SerdataOpsC a = SerdataOpsC
  { sdocEqkey :: FunPtr (SdoEqkey a)
  , sdocGetSize :: FunPtr (SdoGetSize a)
  , sdocFromSer :: FunPtr (SdoFromSer a)
  , sdocFromSerIovec :: FunPtr (SdoFromSerIovec a)
  , sdocFromKeyhash :: FunPtr (SdoFromKeyhash a)
  , sdocFromSample :: FunPtr (SdoFromSample a)
  , sdocToSer :: FunPtr (SdoToSer a)
  , sdocToSerRef :: FunPtr (SdoToSerRef a)
  , sdocToSerUnref :: FunPtr (SdoToSerUnref a)
  , sdocToSample :: FunPtr (SdoToSample a)
  , sdocToUntyped :: FunPtr (SdoToUntyped a)
  , sdocUntypedToSample :: FunPtr (SdoUntypedToSample a)
  , sdocFree :: FunPtr (SdoFree a)
  , sdocPrint :: FunPtr (SdoPrint a)
  , sdocGetKeyhash :: FunPtr (SdoGetKeyhash a)
  -- we never use these and we set them nullPtrs
  -- we never have a need to peek in a SerdataOps
  -- if CycloneDDS was built without DDS_HAS_SHM it'll just be ignored completely
  , sdocGetSampleSize :: FunPtr (SdoGetSampleSize a)
  , sdocFromIoxBuffer :: FunPtr (SdoFromIoxBuffer a)
  } deriving (Generic)
instance CStorable (SerdataOpsC a)
instance Storable (SerdataOpsC a) where
  sizeOf = cSizeOf
  alignment = cAlignment
  peek = cPeek
  poke = cPoke
newtype SerdataOpsPtr a = SerdataOpsPtr (Ptr (SerdataOpsC a)) deriving (Storable)
instance CStorable (SerdataOpsPtr a) where
  cSizeOf = sizeOf
  cAlignment = alignment
  cPeek = peek
  cPoke = poke
data SerdataOps a = SerdataOps
  { sdoEqkey :: SdoEqkey a
  , sdoGetSize :: SdoGetSize a
  , sdoFromSer :: SdoFromSer a
  , sdoFromSerIovec :: SdoFromSerIovec a
  , sdoFromKeyhash :: SdoFromKeyhash a
  , sdoFromSample :: SdoFromSample a
  , sdoToSer :: SdoToSer a
  , sdoToSerRef :: SdoToSerRef a
  , sdoToSerUnref :: SdoToSerUnref a
  , sdoToSample :: SdoToSample a
  , sdoToUntyped :: SdoToUntyped a
  , sdoUntypedToSample :: SdoUntypedToSample a
  , sdoFree :: SdoFree a
  , sdoPrint :: SdoPrint a
  , sdoGetKeyhash :: SdoGetKeyhash a
  }
newSerdataOpsPtr :: SerdataOps a -> IO (SerdataOpsPtr a)
newSerdataOpsPtr SerdataOps{..} = do
  sdocEqkey <- mkSdoEqkey sdoEqkey
  sdocGetSize <- mkSdoGetSize sdoGetSize
  sdocFromSer <- mkSdoFromSer sdoFromSer
  sdocFromSerIovec <- mkSdoFromSerIovec sdoFromSerIovec
  sdocFromKeyhash <- mkSdoFromKeyhash sdoFromKeyhash
  sdocFromSample <- mkSdoFromSample sdoFromSample
  sdocToSer <- mkSdoToSer sdoToSer
  sdocToSerRef <- mkSdoToSerRef sdoToSerRef
  sdocToSerUnref <- mkSdoToSerUnref sdoToSerUnref
  sdocToSample <- mkSdoToSample sdoToSample
  sdocToUntyped <- mkSdoToUntyped sdoToUntyped
  sdocUntypedToSample <- mkSdoUntypedToSample sdoUntypedToSample
  sdocFree <- mkSdoFree sdoFree
  sdocPrint <- mkSdoPrint sdoPrint
  sdocGetKeyhash <- mkSdoGetKeyhash sdoGetKeyhash
  let sdocGetSampleSize = nullFunPtr
      sdocFromIoxBuffer = nullFunPtr
  sdptr <- malloc
  poke sdptr $ SerdataOpsC{..}
  pure $ SerdataOpsPtr sdptr
freeSerdataOpsPtr :: SerdataOpsPtr a -> IO ()
freeSerdataOpsPtr (SerdataOpsPtr sdptr) = do
  sd <- peek sdptr
  freeHaskellFunPtr $ sdocEqkey sd
  freeHaskellFunPtr $ sdocGetSize sd
  freeHaskellFunPtr $ sdocFromSer sd
  freeHaskellFunPtr $ sdocFromSerIovec sd
  freeHaskellFunPtr $ sdocFromKeyhash sd
  freeHaskellFunPtr $ sdocFromSample sd
  freeHaskellFunPtr $ sdocToSer sd
  freeHaskellFunPtr $ sdocToSerRef sd
  freeHaskellFunPtr $ sdocToSerUnref sd
  freeHaskellFunPtr $ sdocToSample sd
  freeHaskellFunPtr $ sdocToUntyped sd
  freeHaskellFunPtr $ sdocUntypedToSample sd
  freeHaskellFunPtr $ sdocFree sd
  freeHaskellFunPtr $ sdocPrint sd
  freeHaskellFunPtr $ sdocGetKeyhash sd
  free sdptr

type StoFree a = Ptr (Sertype a) -> IO ()
type StoReallocSamples a = Ptr (Ptr (SamplesBlob a)) -> Ptr (Sertype a) -> Ptr (SamplesBlob a) -> CSize -> CSize -> IO ()
type StoZeroSamples a = Ptr (Sertype a) -> Ptr (SamplesBlob a) -> CSize -> IO ()
type StoFreeSamples a = Ptr (Sertype a) -> Ptr (Ptr (SamplesBlob a)) -> CSize -> CInt -> IO () -- FIXME dds_free_op_t (final arg)
type StoEqual a = Ptr (Sertype a) -> Ptr (Sertype a) -> Bool
type StoHash a = Ptr (Sertype a) -> Word32
type StoTypeidHash = ()
type StoSerializedSize = ()
type StoSerialize = ()
type StoDeserialize = ()
type StoAssignableFrom = ()

data SertypeV0
data SertypeOpsC a = SertypeOpsC
  { stocVersion :: FunPtr (Ptr SertypeV0) -- forget about Windows for now
  , stocArg :: Ptr ()
  , stocFree :: FunPtr (StoFree a)
  , stocZeroSamples :: FunPtr (StoZeroSamples a)
  , stocReallocSamples :: FunPtr (StoReallocSamples a)
  , stocFreeSamples :: FunPtr (StoFreeSamples a)
  , stocEqual :: FunPtr (StoEqual a)
  , stocHash :: FunPtr (StoHash a)
  , stocTypeidHash :: FunPtr StoTypeidHash
  , stocSerializedSize :: FunPtr StoSerializedSize
  , stocSerialize :: FunPtr StoSerialize
  , stocDeserialize :: FunPtr StoDeserialize
  , stocAssignableFrom :: FunPtr StoAssignableFrom
  } deriving (Generic)
instance CStorable (SertypeOpsC a)
instance Storable (SertypeOpsC a) where
  sizeOf = cSizeOf
  alignment = cAlignment
  peek = cPeek
  poke = cPoke
newtype SertypeOpsPtr a = SertypeOpsPtr (Ptr (SertypeOpsC a)) deriving (Storable)
instance CStorable (SertypeOpsPtr a) where
  cSizeOf = sizeOf
  cAlignment = alignment
  cPeek = peek
  cPoke = poke
data SertypeOps a = SertypeOps
  { stoFree :: StoFree a
  , stoZeroSamples :: StoZeroSamples a
  , stoReallocSamples :: StoReallocSamples a
  , stoFreeSamples :: StoFreeSamples a
  , stoEqual :: StoEqual a
  , stoHash :: StoHash a
  } deriving (Generic)
newSertypeOpsPtr :: SertypeOps a -> IO (SertypeOpsPtr a)
newSertypeOpsPtr SertypeOps{..} = do
  let stocVersion = c_ddsi_sertype_v0
      stocArg = nullPtr
  stocFree <- mkStoFree stoFree
  stocZeroSamples <- mkStoZeroSamples stoZeroSamples
  stocReallocSamples <- mkStoReallocSamples stoReallocSamples
  stocFreeSamples <- mkStoFreeSamples stoFreeSamples
  stocEqual <- mkStoEqual stoEqual
  stocHash <- mkStoHash stoHash
  let stocTypeidHash = nullFunPtr
      stocSerializedSize = nullFunPtr
      stocSerialize = nullFunPtr
      stocDeserialize = nullFunPtr
      stocAssignableFrom = nullFunPtr
  stptr <- malloc
  poke stptr $ SertypeOpsC{..}
  pure $ SertypeOpsPtr stptr
freeSertypeOpsPtr :: SertypeOpsPtr a -> IO ()
freeSertypeOpsPtr (SertypeOpsPtr stptr) = do
  st <- peek stptr
  freeHaskellFunPtr $ stocFree st
  freeHaskellFunPtr $ stocZeroSamples st
  freeHaskellFunPtr $ stocReallocSamples st
  freeHaskellFunPtr $ stocFreeSamples st
  freeHaskellFunPtr $ stocEqual st
  freeHaskellFunPtr $ stocHash st
  free stptr

foreign import ccall "&ddsi_sertype_v0" c_ddsi_sertype_v0 :: FunPtr (Ptr SertypeV0)

foreign import ccall "wrapper" mkStoFree :: StoFree a -> IO (FunPtr (StoFree a))
foreign import ccall "wrapper" mkStoZeroSamples :: StoZeroSamples a -> IO (FunPtr (StoZeroSamples a))
foreign import ccall "wrapper" mkStoReallocSamples :: StoReallocSamples a -> IO (FunPtr (StoReallocSamples a))
foreign import ccall "wrapper" mkStoFreeSamples :: StoFreeSamples a -> IO (FunPtr (StoFreeSamples a))
foreign import ccall "wrapper" mkStoEqual :: StoEqual a -> IO (FunPtr (StoEqual a))
foreign import ccall "wrapper" mkStoHash :: StoHash a -> IO (FunPtr (StoHash a))

foreign import ccall "wrapper" mkSdoEqkey :: SdoEqkey a -> IO (FunPtr (SdoEqkey a))
foreign import ccall "wrapper" mkSdoGetSize :: SdoGetSize a -> IO (FunPtr (SdoGetSize a))
foreign import ccall "wrapper" mkSdoFromSer :: SdoFromSer a -> IO (FunPtr (SdoFromSer a))
foreign import ccall "wrapper" mkSdoFromSerIovec :: SdoFromSerIovec a -> IO (FunPtr (SdoFromSerIovec a))
foreign import ccall "wrapper" mkSdoFromKeyhash :: SdoFromKeyhash a -> IO (FunPtr (SdoFromKeyhash a))
foreign import ccall "wrapper" mkSdoFromSample :: SdoFromSample a -> IO (FunPtr (SdoFromSample a))
foreign import ccall "wrapper" mkSdoToSer :: SdoToSer a -> IO (FunPtr (SdoToSer a))
foreign import ccall "wrapper" mkSdoToSerRef :: SdoToSerRef a -> IO (FunPtr (SdoToSerRef a))
foreign import ccall "wrapper" mkSdoToSerUnref :: SdoToSerUnref a -> IO (FunPtr (SdoToSerUnref a))
foreign import ccall "wrapper" mkSdoToSample :: SdoToSample a -> IO (FunPtr (SdoToSample a))
foreign import ccall "wrapper" mkSdoToUntyped :: SdoToUntyped a -> IO (FunPtr (SdoToUntyped a))
foreign import ccall "wrapper" mkSdoUntypedToSample :: SdoUntypedToSample a -> IO (FunPtr (SdoUntypedToSample a))
foreign import ccall "wrapper" mkSdoFree :: SdoFree a -> IO (FunPtr (SdoFree a))
foreign import ccall "wrapper" mkSdoPrint :: SdoPrint a -> IO (FunPtr (SdoPrint a))
foreign import ccall "wrapper" mkSdoGetKeyhash :: SdoGetKeyhash a -> IO (FunPtr (SdoGetKeyhash a))

newtype SertypeInitFlags = SertypeInitFlags { unSertypeInitFlags :: Word32 }
  deriving (Num, Bounded, Show, Storable)
sertypeTopicKindNoKey, sertypeRequestKeyhash, sertypeFixedSize :: SertypeInitFlags
sertypeTopicKindNoKey = SertypeInitFlags 1
sertypeRequestKeyhash = SertypeInitFlags 2
sertypeFixedSize      = SertypeInitFlags 4

foreign import capi "dds/ddsi/ddsi_serdata.h ddsi_sertype_init_flags"
  c_ddsi_sertype_init_flags :: Ptr (Sertype a) -> CString -> SertypeOpsPtr a -> SerdataOpsPtr a -> SertypeInitFlags -> IO ()
foreign import capi "dds/ddsi/ddsi_serdata.h ddsi_serdata_init"
  c_ddsi_serdata_init :: Ptr (Serdata a) -> Ptr (Sertype a) -> SerdataKindInt -> IO ()
foreign import capi "dds/ddsi/ddsi_serdata.h ddsi_serdata_ref"
  c_ddsi_serdata_ref :: Ptr (Serdata a) -> IO (Ptr (Serdata a))
foreign import capi "dds/ddsi/ddsi_serdata.h ddsi_serdata_unref"
  c_ddsi_serdata_unref :: Ptr (Serdata a) -> IO ()
