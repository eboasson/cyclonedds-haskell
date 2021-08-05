{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}

module CycloneDDS (
  TopicDescriptor,
  Handle,
  Participant,
  Topic,
  Writer,
  delete,
  write,
  c_dds_create_participant,
  c_dds_create_topic,
  c_dds_create_writer,
  --c_dds_write,
  c_dds_delete,
  OneULong(..),
  topicdesc_OneULong) where
  
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Data.Word
import Control.Monad

data TopicDescriptor a
newtype Handle = Handle CInt deriving (Eq, Ord, Show)
newtype Participant = Participant { unParticipant :: Handle }
newtype Topic a = Topic { unTopic :: Handle }
newtype Writer a = Writer { unWriter :: Handle }

class Entity a where
  entityHandle :: a -> Handle
  delete :: a -> IO CInt
  delete = c_dds_delete . entityHandle
instance Entity Participant where
  entityHandle = unParticipant
instance Entity (Topic a) where
  entityHandle = unTopic
instance Entity (Writer a) where
  entityHandle = unWriter

foreign import capi "dds/dds.h dds_create_participant"
  c_dds_create_participant :: CInt -> Ptr () -> Ptr () -> IO Participant
foreign import capi "dds/dds.h dds_create_topic"
  c_dds_create_topic :: Participant -> Ptr (TopicDescriptor a) -> CString -> Ptr () -> Ptr () -> IO (Topic a)
foreign import capi "dds/dds.h dds_create_writer"
  c_dds_create_writer :: Participant -> Topic a -> Ptr () -> Ptr () -> IO (Writer a)
foreign import capi "dds/dds.h dds_write"
  c_dds_write :: Writer a -> Ptr a -> IO CInt
foreign import capi "dds/dds.h dds_delete"
  c_dds_delete :: Handle -> IO CInt

write :: Storable a => Writer a -> a -> IO CInt
write wr v = alloca $ \ptr -> poke ptr v >> c_dds_write wr ptr

-- Make ddsperf's OneULong type available in Haskell relying on IDLC to generate a topic
-- descriptor.  Writing/reading requires marshalling to/from the C representation of the
-- type, rather than going to/from CDR directly.

foreign import ccall "&OneULong_desc" topicdesc_OneULong :: Ptr (TopicDescriptor OneULong)
data OneULong = OneULong { oneULongSeq :: Word32 } deriving (Show)
instance Storable OneULong where
  sizeOf _ = sizeOf (undefined :: Word32)
  alignment _ = alignment (undefined :: Word32)
  peek = liftM OneULong . peek . castPtr
  poke p = poke (castPtr p) . oneULongSeq
