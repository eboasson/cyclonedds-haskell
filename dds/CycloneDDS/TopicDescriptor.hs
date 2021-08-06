module CycloneDDS.TopicDescriptor (
  TopicDescriptor,
  AllocatingStorable,
  topicDescriptor,
  freeAllocatedMemory,
  TopicDescriptorPtr(..)) where

import Foreign.Ptr
import Foreign.Storable
import Data.Proxy

class Storable a => AllocatingStorable a where
  freeAllocatedMemory :: Ptr a -> IO ()

newtype TopicDescriptorPtr a = TopicDescriptorPtr (Ptr a)

class AllocatingStorable a => TopicDescriptor a where
  topicDescriptor :: Proxy a -> TopicDescriptorPtr a
