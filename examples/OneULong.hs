{-# LANGUAGE ForeignFunctionInterface #-}

module OneULong (OneULong(..)) where

import Foreign.Storable
import Data.Word

import CycloneDDS.TopicDescriptor

-- Make ddsperf's OneULong type available in Haskell relying on IDLC to generate a topic
-- descriptor.  Writing/reading requires marshalling to/from the C representation of the
-- type, rather than going to/from CDR directly.
--
-- OneULong is simple enough to do this by hand!

data OneULong = OneULong
  { oneULongSeq :: Word32
  } deriving (Show)

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
