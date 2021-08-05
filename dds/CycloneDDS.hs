{-# LANGUAGE CApiFFI #-}

module CycloneDDS (
  DomainId(..),
  Handle,
  Publisher,
  Participant,
  Topic,
  Writer,
  delete,
  write,
  newParticipant,
  newPublisher,
  newTopic,
  newWriter) where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Data.Word
import Data.Proxy
import TopicDescriptor

data DomainId = DefaultDomainId | DomainId Word32 deriving (Eq, Ord, Show, Read)
newtype Handle = Handle CInt deriving (Eq, Ord, Show, Read)
newtype Participant = Participant { unParticipant :: Handle } deriving (Eq, Ord, Show, Read)
newtype Publisher = Publisher { unPublisher :: Handle } deriving (Eq, Ord, Show, Read)
newtype Topic a = Topic { unTopic :: Handle } deriving (Eq, Ord, Show, Read)
newtype Writer a = Writer { unWriter :: Handle } deriving (Eq, Ord, Show, Read)

class Entity a where
  entityHandle :: a -> Handle
  delete :: a -> IO CInt
  delete = c_dds_delete . entityHandle
instance Entity Participant where
  entityHandle = unParticipant
instance Entity Publisher where
  entityHandle = unPublisher
instance Entity (Topic a) where
  entityHandle = unTopic
instance Entity (Writer a) where
  entityHandle = unWriter

newParticipant :: DomainId -> IO Participant
newParticipant DefaultDomainId = newParticipant (DomainId 0xffffffff)
newParticipant (DomainId did) = c_dds_create_participant did nullPtr nullPtr

newTopic :: TopicDescriptor a => Participant -> String -> IO (Topic a)
newTopic dp name = withCString name $ \cname -> c_dds_create_topic (entityHandle dp) td cname nullPtr nullPtr
  where td = topicDescriptor (Proxy :: Proxy a)

newPublisher :: Participant -> IO Publisher
newPublisher dp = c_dds_create_publisher (entityHandle dp) nullPtr nullPtr

class Entity a => CreateWriterCapable a where
  newWriter :: a -> Topic b -> IO (Writer b)
  newWriter p tp = c_dds_create_writer (entityHandle p) tp nullPtr nullPtr
instance CreateWriterCapable Participant
instance CreateWriterCapable Publisher

write :: Storable a => Writer a -> a -> IO CInt
write wr v = alloca $ \ptr -> poke ptr v >> c_dds_write wr ptr

foreign import capi "dds/dds.h dds_create_participant"
  c_dds_create_participant :: Word32 -> Ptr () -> Ptr () -> IO Participant
foreign import capi "dds/dds.h dds_create_topic"
  c_dds_create_topic :: Handle -> TopicDescriptorPtr a -> CString -> Ptr () -> Ptr () -> IO (Topic a)
foreign import capi "dds/dds.h dds_create_publisher"
  c_dds_create_publisher :: Handle -> Ptr () -> Ptr () -> IO Publisher
foreign import capi "dds/dds.h dds_create_writer"
  c_dds_create_writer :: Handle -> Topic a -> Ptr () -> Ptr () -> IO (Writer a)
foreign import capi "dds/dds.h dds_write"
  c_dds_write :: Writer a -> Ptr a -> IO CInt
foreign import capi "dds/dds.h dds_delete"
  c_dds_delete :: Handle -> IO CInt
