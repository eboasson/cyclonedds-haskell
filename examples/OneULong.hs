{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module OneULong (OneULong(..)) where

import Foreign.Storable
import Data.Word

import GHC.Generics (Generic)
import Data.ByteString.Builder
import qualified Data.Serialize
import CycloneDDS.Serdata
import qualified Data.ByteString.Lazy as BL

import CycloneDDS.TopicDescriptor

-- Make ddsperf's OneULong type available in Haskell relying on IDLC to generate a topic
-- descriptor.  Writing/reading requires marshalling to/from the C representation of the
-- type, rather than going to/from CDR directly.
--
-- OneULong is simple enough to do this by hand!

data OneULong = OneULong
  { oneULongSeq :: Word32
  } deriving (Show, Generic)
instance Data.Serialize.Serialize OneULong

instance TopicType OneULong where
  type TopicKey OneULong = ()
  typeName = const "OneULong"
  hasKey = const False
  getKey = const ()
  encodeKey = const BL.empty
  encodeSample a = toLazyByteString $ word16BE 0 `mappend` word16BE 0 `mappend` (byteString . Data.Serialize.encode) a
  decodeKey = const $ Right ()
  decodeSample = Data.Serialize.decodeLazy

instance Storable OneULong where
  sizeOf _ = sizeOf (undefined :: Word32)
  alignment _ = alignment (undefined :: Word32)
  peek ptr = OneULong <$> peekByteOff ptr 0
  poke ptr (OneULong a) = pokeByteOff ptr 0 a

instance AllocatingStorable OneULong where
  freeAllocatedMemory _ = pure ()

instance TopicDescriptor OneULong where
  topicDescriptor _ = c_OneULong_desc

foreign import ccall "&OneULong_desc" c_OneULong_desc :: TopicDescriptorPtr OneULong
