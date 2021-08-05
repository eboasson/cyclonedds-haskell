{-# LANGUAGE ForeignFunctionInterface #-}

module OneULong (OneULong(..)) where

import Foreign.Ptr
import Foreign.Storable
import Data.Word
import Control.Monad
import TopicDescriptor

-- Make ddsperf's OneULong type available in Haskell relying on IDLC to generate a topic
-- descriptor.  Writing/reading requires marshalling to/from the C representation of the
-- type, rather than going to/from CDR directly.
--
-- OneULong is simple enough to do this by hand!

foreign import ccall "&OneULong_desc" c_OneULong_desc :: TopicDescriptorPtr OneULong
data OneULong = OneULong { oneULongSeq :: Word32 } deriving (Show)
instance Storable OneULong where
  sizeOf _ = sizeOf (undefined :: Word32)
  alignment _ = alignment (undefined :: Word32)
  peek = liftM OneULong . peek . castPtr
  poke p = poke (castPtr p) . oneULongSeq
instance TopicDescriptor OneULong where
  topicDescriptor _ = c_OneULong_desc
