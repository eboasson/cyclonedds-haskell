{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CycloneDDS.Qos
  (Qos, QosPolicy(..),
   DurabilityKind(..),
   AccessScopeKind(..),
   OwnershipKind(..),
   LivelinessKind(..),
   OrderKind(..),
   HistoryKind(..),
   ReliabilityKind(..),
   QosObject(..), -- need constructor for marshalling in foreign import in CycloneDDS.hs ...
   newQos,
   freeQos
  ) where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Data.Int
import Data.Word
import Control.Monad (forM_)

data DurabilityKind = Volatile | TransientLocal | Transient | Persistent deriving (Enum, Read, Show, Eq, Ord)
data AccessScopeKind = ByInstance | ByTopic | ByGroup deriving (Enum, Read, Show, Eq, Ord)
data OwnershipKind = Shared | Exclusive deriving (Enum, Read, Show, Eq, Ord)
data LivelinessKind = AutomaticLiveliness | ManualByParticipant | ManualByTopic deriving (Enum, Read, Show, Eq, Ord)
data OrderKind = ByReception | BySource deriving (Enum, Read, Show, Eq, Ord)
data HistoryKind = KeepLast Int | KeepAll deriving (Read, Show, Eq, Ord)
data ReliabilityKind = BestEffort | Reliable deriving (Enum, Read, Show, Eq, Ord)

data QosPolicy = UserData [Word8]
               | TopicData [Word8]
               | GroupData [Word8]
               | Priority Integer
               | Lifespan Double
               | Durability DurabilityKind
               | AccessScope AccessScopeKind
               | CoherentAccess Bool
               | OrderedAccess Bool
               | Deadline Double
               | LatencyBudget Double
               | Ownership OwnershipKind
               | Strength Integer
               | Liveliness LivelinessKind
               | LeaseDuration Double
               | TimeBasedFilter Double
               | Partition [String]
               | Reliability ReliabilityKind
               | MaxBlockingTime Double
               | Order OrderKind
               | History HistoryKind
               | MaxSamples Integer
               | MaxInstances Integer
               | MaxSamplesPerInstance Integer
               | AutoEnable Bool
               | AutoDispose Bool
               | AutoPurgeSuspendedDelay Double
               | AutoUnregisterDelay Double
               | AutoPurgeNoWritersDelay Double
               | AutoPurgeDisposedDelay Double
               | AutoPurgeDisposeAll Bool
               | ServiceCleanupDelay Double
               | DurabilityService [QosPolicy]
               deriving (Read, Show, Eq)

type Qos = [QosPolicy]

newtype QosObject = QosObject (Ptr QosObject) deriving (Storable)

newQos :: ReliabilityKind -> Qos -> IO QosObject
newQos defrelkind qos = do
  qosobj <- c_dds_create_qos
  forM_ qos $ \pol ->
    case pol of
      Reliability k -> setReliabilityKind qosobj k
      MaxBlockingTime d -> setMaxBlockingTime qosobj defrelkind d
      History k -> setHistory qosobj k
      _ -> pure ()
  pure qosobj

freeQos :: QosObject -> IO ()
freeQos = c_dds_delete_qos

setReliabilityKind :: QosObject -> ReliabilityKind -> IO ()
setReliabilityKind q k = do
  d <- alloca $ \dptr -> do
    b <- c_dds_qget_reliability q nullPtr dptr
    if b then peek dptr else pure 100_000_000_000
  c_dds_qset_reliability q (fromIntegral $ fromEnum k) d

setMaxBlockingTime :: QosObject -> ReliabilityKind -> Double -> IO ()
setMaxBlockingTime q defk dflt = do
  k <- alloca $ \kptr -> do
    b <- c_dds_qget_reliability q kptr nullPtr
    if b then peek kptr else pure (fromIntegral $ fromEnum defk)
  let d = if dflt < 0 then 9223372036854775807 else fromIntegral (floor dflt :: Integer)
  c_dds_qset_reliability q k d

setHistory :: QosObject -> HistoryKind -> IO ()
setHistory q KeepAll = c_dds_qset_history q 1 0
setHistory q (KeepLast n) = c_dds_qset_history q 0 (fromIntegral n)

foreign import capi "dds/dds.h dds_create_qos"
  c_dds_create_qos :: IO QosObject
foreign import capi "dds/dds.h dds_delete_qos"
  c_dds_delete_qos :: QosObject -> IO ()

foreign import capi "dds/dds.h dds_qset_reliability"
  c_dds_qset_reliability :: QosObject -> CInt -> Int64 -> IO ()
foreign import capi "dds/dds.h dds_qset_history"
  c_dds_qset_history :: QosObject -> CInt -> CInt -> IO ()

foreign import capi "dds/dds.h dds_qget_reliability"
  c_dds_qget_reliability :: QosObject -> Ptr CInt -> Ptr Int64 -> IO Bool
