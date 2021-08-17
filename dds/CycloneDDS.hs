{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CycloneDDS (
  DomainId(..),
  Handle,
  Publisher,
  Participant,
  Topic,
  Writer,
  delete,
  write,
  takeN,
  newParticipant,
  newPublisher,
  newSubscriber,
  newTopic,
  newTopic',
  write',
  takeN',
  takeN'',
  newWriter,
  newReader,
  Sample(..),
  TopicType(..),
  InstanceHandle,
  IState(..),
  SampleInfo(..),
  Qos, QosPolicy(..),
  DurabilityKind(..), AccessScopeKind(..), OwnershipKind(..), LivelinessKind(..),
  OrderKind(..), HistoryKind(..), ReliabilityKind(..)
  ) where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.StablePtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Control.Monad (when)
import Data.Int
import Data.Word
import Data.Proxy
import Data.IORef
import CycloneDDS.TopicDescriptor
import CycloneDDS.BaseTypes
import CycloneDDS.SampleInfo
import CycloneDDS.Qos
import CycloneDDS.Serdata
import CycloneDDS.Sample

data DomainId = DefaultDomainId | DomainId Word32 deriving (Eq, Ord, Show, Read)

data Participant = Participant
  { participantHandle :: Handle ()
  } deriving (Eq, Ord, Show, Read)
data Publisher = Publisher
  { publisherHandle :: Handle ()
  , publisherParent :: Participant
  } deriving (Eq, Ord, Show, Read)
data Subscriber = Subscriber
  { subscriberHandle :: Handle ()
  , subscriberParent :: Participant
  } deriving (Eq, Ord, Show, Read)
data Topic a = Topic
  { topicHandle :: Handle a
  , topicParent :: Participant
  } deriving (Eq, Ord, Show, Read)

data WriterParent = WriterParentPublisher Publisher
                  | WriterParentParticipant Participant
                  deriving (Eq, Ord, Show, Read)

data Writer a = Writer
  { writerHandle :: Handle a
  , writerParent :: WriterParent
  , writerTopic :: Topic a
  } deriving (Eq, Ord, Show, Read)

data ReaderParent = ReaderParentSubscriber Subscriber
                  | ReaderParentParticipant Participant
                  deriving (Eq, Ord, Show, Read)

data Reader a = Reader
  { readerHandle :: Handle a
  , readerParent :: ReaderParent
  , readerTopic :: Topic a
  } deriving (Eq, Ord, Show, Read)

eraseHandleType :: Handle a -> Handle ()
eraseHandleType = Handle. unHandle

class Entity a where
  entityHandle :: a -> Handle ()
  delete :: a -> IO Int32
  delete = c_dds_delete . entityHandle
instance Entity Participant where
  entityHandle = participantHandle
instance Entity Publisher where
  entityHandle = publisherHandle
instance Entity Subscriber where
  entityHandle = subscriberHandle
instance Entity (Topic a) where
  entityHandle = eraseHandleType . topicHandle
instance Entity (Writer a) where
  entityHandle = eraseHandleType . writerHandle
instance Entity (Reader a) where
  entityHandle = eraseHandleType . readerHandle

instance Entity WriterParent where
  entityHandle (WriterParentPublisher pub) = entityHandle pub
  entityHandle (WriterParentParticipant dp) = entityHandle dp
instance Entity ReaderParent where
  entityHandle (ReaderParentSubscriber sub) = entityHandle sub
  entityHandle (ReaderParentParticipant dp) = entityHandle dp

newParticipant :: DomainId -> IO Participant
newParticipant DefaultDomainId = newParticipant (DomainId 0xffffffff)
newParticipant (DomainId did) = do
  dp <- c_dds_create_participant did nullPtr nullPtr
  return $ Participant { participantHandle = dp }

newTopic :: TopicDescriptor a => Participant -> Qos -> String -> IO (Topic a)
newTopic dp qos name = do
  let dph = entityHandle dp
      td = topicDescriptor Proxy
  qosobj <- newQos BestEffort qos -- weird DDS spec has default QoS for a topic set to best-effort ...
  tp <- withCString name $ \cname -> c_dds_create_topic dph td cname qosobj nullPtr
  freeQos qosobj
  return $ Topic { topicHandle = tp, topicParent = dp }

newTopic' :: forall a. (TopicType a, Show a) => Participant -> Qos -> String -> IO (Topic a)
newTopic' dp qos name = do
  let dph = entityHandle dp
      tn = typeName (Proxy :: Proxy a)
  sertypeptr <- newSertype Proxy tn
  sertypeptrptr <- malloc
  poke sertypeptrptr sertypeptr
  qosobj <- newQos BestEffort qos -- weird DDS spec has default QoS for a topic set to best-effort ...
  tp <- withCString name $ \cname -> c_dds_create_topic_sertype dph cname sertypeptrptr qosobj nullPtr nullPtr
  freeQos qosobj
  free sertypeptrptr -- how do I keep the sertype alive? StablePtr? Store it in the topic? That's probably better
  return $ Topic { topicHandle = tp, topicParent = dp }

newPublisher :: Participant -> IO Publisher
newPublisher dp = do
  pub <- c_dds_create_publisher (entityHandle dp) nullPtr nullPtr
  return $ Publisher { publisherHandle = pub, publisherParent = dp }

newSubscriber :: Participant -> IO Subscriber
newSubscriber dp = do
  sub <- c_dds_create_subscriber (entityHandle dp) nullPtr nullPtr
  return $ Subscriber { subscriberHandle = sub, subscriberParent = dp }

class Entity a => CreateWriterCapable a where
  getWriterParent :: a -> WriterParent
instance CreateWriterCapable Participant where
  getWriterParent dp = WriterParentParticipant dp
instance CreateWriterCapable Publisher where
  getWriterParent pub = WriterParentPublisher pub

newWriter :: CreateWriterCapable a => a -> Topic b -> IO (Writer b)
newWriter wp tp = do
  wr <- c_dds_create_writer (entityHandle wp) (topicHandle tp) nullPtr nullPtr
  return $ Writer { writerHandle = wr, writerParent = getWriterParent wp, writerTopic = tp }

class Entity a => CreateReaderCapable a where
  getReaderParent :: a -> ReaderParent
instance CreateReaderCapable Participant where
  getReaderParent dp = ReaderParentParticipant dp
instance CreateReaderCapable Subscriber where
  getReaderParent sub = ReaderParentSubscriber sub

newReader :: CreateReaderCapable a => a -> Topic b -> IO (Reader b)
newReader rp tp = do
  rd <- c_dds_create_reader (entityHandle rp) (topicHandle tp) nullPtr nullPtr
  return $ Reader { readerHandle = rd, readerParent = getReaderParent rp, readerTopic = tp }

-- AllocatingStorable is but a quick hack
write :: AllocatingStorable a => Writer a -> a -> IO Int32
write wr v = alloca $ \ptr -> do
  poke ptr v
  ret <- c_dds_write (writerHandle wr) ptr
  freeAllocatedMemory ptr
  pure ret

write' :: TopicType a => Writer a -> a -> IO Int32
write' wr v = do
  stablev <- SamplePtr <$> newStablePtr v
  ret <- c_dds_write_sampleptr (writerHandle wr) stablev
  freeStablePtr $ unSamplePtr $ stablev
  pure ret

takeN :: Storable a => Int -> Reader a -> IO [(SampleInfo, a)]
takeN maxn rd
  | maxn <= 0 = pure $ []
  | otherwise = do
      allocaArray maxn $ \siptr ->
        allocaArray maxn $ \vsxptr -> do
          poke vsxptr nullPtr -- null pointer in first elemnt of data pointers -> C API mallocs for us
          n <- c_dds_take (readerHandle rd) vsxptr siptr (fromIntegral maxn) (fromIntegral maxn)
          when (n < 0) $ fail ("error in takeN/dds_take: " ++ show n)
          si <- peekArray (fromIntegral n) siptr
          vsx <- peekArray (fromIntegral n) vsxptr
          vs <- mapM peek vsx -- FIXME: this only kinda works for invalid data
          _ <- c_dds_return_loan (readerHandle rd) vsxptr n
          pure $ zip si vs -- could do it in one step, peeking arrays in parallel

takeN' :: TopicType a => Int -> Reader a -> IO [(SampleInfo, Maybe a)]
takeN' maxn rd
  | maxn <= 0 = pure $ []
  | otherwise = do
      allocaArray maxn $ \siptr ->
        allocaArray maxn $ \vsxptr -> do
          poke vsxptr nullPtr -- null pointer in first elemnt of data pointers -> C API calls serdataReallocSamples
          let cptrs = castPtr vsxptr
          n <- c_dds_take (readerHandle rd) cptrs siptr (fromIntegral maxn) (fromIntegral maxn)
          when (n < 0) $ fail ("error in takeN/dds_take: " ++ show n)
          si <- peekArray (fromIntegral n) siptr
          blobptr <- peek vsxptr
          blob <- peek blobptr
          ioref <- deRefStablePtr blob
          vs <- readIORef ioref
          _ <- c_dds_return_loan (readerHandle rd) cptrs n
          pure $ zip si (reverse vs)

takeN'' :: TopicType a => Int -> Reader a -> IO [(SampleInfo, Sample a)]
takeN'' maxn rd
  | maxn <= 0 = pure $ []
  | otherwise = do
      allocaArray maxn $ \siptr ->
        allocaArray maxn $ \serdataptrs1 -> do
          n <- c_dds_takecdr (readerHandle rd) serdataptrs1 (fromIntegral maxn) siptr 127 -- any sample/view/instance state
          when (n < 0) $ fail ("error in takeN/dds_take: " ++ show n)
          si <- peekArray (fromIntegral n) siptr
          serdataptrs <- peekArray (fromIntegral n) serdataptrs1
          serdatas <- mapM peek serdataptrs
          vsc <- mapM serdataContent serdatas
          let vs = map sdSample vsc
          mapM_ c_ddsi_serdata_unref serdataptrs
          pure $ zip si vs

foreign import capi "dds/dds.h dds_create_participant"
  c_dds_create_participant :: Word32 -> Ptr () -> Ptr () -> IO (Handle ())
foreign import capi "dds/dds.h dds_create_topic"
  c_dds_create_topic :: Handle a -> TopicDescriptorPtr b -> CString -> QosObject -> Ptr () -> IO (Handle b)
foreign import capi "dds/dds.h dds_create_topic_sertype"
  c_dds_create_topic_sertype :: Handle a -> CString -> Ptr (Ptr (Sertype b)) -> QosObject -> Ptr () -> Ptr () -> IO (Handle b)
foreign import capi "dds/dds.h dds_create_publisher"
  c_dds_create_publisher :: Handle a -> Ptr () -> Ptr () -> IO (Handle ())
foreign import capi "dds/dds.h dds_create_subscriber"
  c_dds_create_subscriber :: Handle a -> Ptr () -> Ptr () -> IO (Handle ())
foreign import capi "dds/dds.h dds_create_writer"
  c_dds_create_writer :: Handle a -> Handle b -> Ptr () -> Ptr () -> IO (Handle b)
foreign import capi "dds/dds.h dds_create_reader"
  c_dds_create_reader :: Handle a -> Handle b -> Ptr () -> Ptr () -> IO (Handle b)
foreign import capi "dds/dds.h dds_write"
  c_dds_write :: Handle a -> Ptr a -> IO Int32
foreign import capi "dds/dds.h dds_write"
  c_dds_write_sampleptr :: Handle a -> SamplePtr a -> IO Int32
foreign import capi "dds/dds.h dds_take"
  c_dds_take :: Handle a -> Ptr (Ptr a) -> Ptr SampleInfo -> CSize -> Word32 -> IO Int32
foreign import capi "dds/dds.h dds_takecdr"
  c_dds_takecdr :: Handle a -> Ptr (Ptr (Serdata a)) -> CSize -> Ptr SampleInfo -> Word32 -> IO Int32
foreign import capi "dds/dds.h dds_return_loan"
  c_dds_return_loan :: Handle a -> Ptr (Ptr a) -> Int32 -> IO Int32
foreign import capi "dds/dds.h dds_delete"
  c_dds_delete :: Handle a -> IO Int32
