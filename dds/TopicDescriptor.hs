module TopicDescriptor (
  TopicDescriptor,
  topicDescriptor,
  TopicDescriptorPtr(..)) where

import Foreign.Ptr
import Foreign.Storable
import Data.Proxy

newtype TopicDescriptorPtr a = TopicDescriptorPtr (Ptr a)
class Storable a => TopicDescriptor a where
  topicDescriptor :: Proxy a -> TopicDescriptorPtr a
