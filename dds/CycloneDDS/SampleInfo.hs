module CycloneDDS.SampleInfo (
  SampleInfo(..),
  IState(..)) where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Data.Word
import CycloneDDS.BaseTypes

{-
#define DDS_READ_SAMPLE_STATE 1u
#define DDS_NOT_READ_SAMPLE_STATE 2u
#define DDS_ANY_SAMPLE_STATE (1u | 2u)

#define DDS_NEW_VIEW_STATE 4u
#define DDS_NOT_NEW_VIEW_STATE 8u
#define DDS_ANY_VIEW_STATE (4u | 8u)

#define DDS_ALIVE_INSTANCE_STATE 16u
#define DDS_NOT_ALIVE_DISPOSED_INSTANCE_STATE 32u
#define DDS_NOT_ALIVE_NO_WRITERS_INSTANCE_STATE 64u
#define DDS_ANY_INSTANCE_STATE (16u | 32u | 64u)

enum dds_sample_state
  DDS_SST_READ = DDS_READ_SAMPLE_STATE,
  DDS_SST_NOT_READ = DDS_NOT_READ_SAMPLE_STATE

enum dds_view_state
  DDS_VST_NEW = DDS_NEW_VIEW_STATE,
  DDS_VST_OLD = DDS_NOT_NEW_VIEW_STATE

enum dds_instance_state
  DDS_IST_ALIVE = DDS_ALIVE_INSTANCE_STATE,
  DDS_IST_NOT_ALIVE_DISPOSED = DDS_NOT_ALIVE_DISPOSED_INSTANCE_STATE,
  DDS_IST_NOT_ALIVE_NO_WRITERS = DDS_NOT_ALIVE_NO_WRITERS_INSTANCE_STATE

struct dds_sample_info
  dds_sample_state_t sample_state;
  dds_view_state_t view_state;
  dds_instance_state_t instance_state;
  bool valid_data;
  dds_time_t source_timestamp;
  dds_instance_handle_t instance_handle;
  dds_instance_handle_t publication_handle;
  uint32_t disposed_generation_count;
  uint32_t no_writers_generation_count;
  uint32_t sample_rank;
  uint32_t generation_rank;
  uint32_t absolute_generation_rank;
-}

data SState = Read | Unread deriving (Eq, Ord, Enum, Show, Read)
instance Storable SState where
  sizeOf _ = sizeOf (undefined :: CInt)
  alignment _ = alignment (undefined :: CInt)
  peek ptr = peek (castPtr ptr :: Ptr CInt) >>= \v -> pure $ case v of
    1 -> Read
    2 -> Unread
    _ -> error ("invalid sample_state encountered in dds_sample_info_t: " ++ show v)
  poke ptr v = poke (castPtr ptr :: Ptr CInt) $ case v of
    Read -> 1
    Unread -> 2

data VState = Old | New deriving (Eq, Ord, Enum, Show, Read)
instance Storable VState where
  sizeOf _ = sizeOf (undefined :: CInt)
  alignment _ = alignment (undefined :: CInt)
  peek ptr = peek (castPtr ptr :: Ptr CInt) >>= \v -> pure $ case v of
    4 -> New
    8 -> Old
    _ -> error ("invalid view_state encountered in dds_sample_info_t: " ++ show v)
  poke ptr v = poke (castPtr ptr :: Ptr CInt) $ case v of
    New -> 4
    Old -> 8

data IState = Alive | Disposed | NoWriters deriving (Eq, Ord, Enum, Show, Read)
instance Storable IState where
  sizeOf _ = sizeOf (undefined :: CInt)
  alignment _ = alignment (undefined :: CInt)
  peek ptr = peek (castPtr ptr :: Ptr CInt) >>= \v -> pure $ case v of
    16 -> Alive
    32 -> Disposed
    64 -> NoWriters
    _  -> error ("invalid instance_state encountered in dds_sample_info_t: " ++ show v)
  poke ptr v = poke (castPtr ptr :: Ptr CInt) $ case v of
    Alive -> 16
    Disposed -> 32
    NoWriters -> 64

data SampleInfo = SampleInfo
  { siUnread :: Bool
  , siNew :: Bool
  , siIState :: IState
  , siValid :: Bool
  , siTimestamp :: Integer
  , siInstanceHandle :: InstanceHandle
  , siPublicationHandle :: InstanceHandle
  , siDisposedGen :: Word32
  , siNoWritersGen :: Word32
  , siSampleRank :: Word32
  , siGenerationRank :: Word32
  , siAbsGenerationRank :: Word32
  } deriving (Show)
instance Storable SampleInfo where
  sizeOf _ = 64
  alignment _ = alignment (undefined :: InstanceHandle)
  peek ptr = SampleInfo
    <$> (peekByteOff ptr 0 >>= pure . toUnread)     -- sample_state
    <*> (peekByteOff ptr 4 >>= pure . toNew)        -- view_state
    <*> peekByteOff ptr 8                           -- instance_state
    <*> peekByteOff ptr 12                          -- valid_data
    <*> (peekByteOff ptr 16 >>= pure . toTimestamp) -- source_timestamp
    <*> peekByteOff ptr 24                          -- instance_handle
    <*> peekByteOff ptr 32                          -- publication_handle
    <*> peekByteOff ptr 40                          -- disposed_generation_count
    <*> peekByteOff ptr 44                          -- no_writers_generation_count
    <*> peekByteOff ptr 48                          -- sample_rank
    <*> peekByteOff ptr 52                          -- generation_rank
    <*> peekByteOff ptr 56                          -- absolute_generation_rank
    where
      toUnread Read = False
      toUnread Unread = True
      toNew Old = False
      toNew New = True
      toTimestamp :: Word64 -> Integer
      toTimestamp = fromIntegral
  poke ptr (SampleInfo a b c d e f g h i j k l) = do
    pokeByteOff ptr 0 (fromUnread a)     -- sample_state
    pokeByteOff ptr 4 (fromNew b)        -- view_state
    pokeByteOff ptr 8 c                  -- instance_state
    pokeByteOff ptr 12 d                 -- valid_data
    pokeByteOff ptr 16 (fromTimestamp e) -- source_timestamp
    pokeByteOff ptr 24 f                 -- instance_handle
    pokeByteOff ptr 32 g                 -- publication_handle
    pokeByteOff ptr 40 h                 -- disposed_generation_count
    pokeByteOff ptr 44 i                 -- no_writers_generation_count
    pokeByteOff ptr 48 j                 -- sample_rank
    pokeByteOff ptr 52 k                 -- generation_rank
    pokeByteOff ptr 56 l                 -- absolute_generation_rank
    where
      fromUnread False = Read
      fromUnread True = Unread
      fromNew False = Old
      fromNew True = New
      fromTimestamp :: Integer -> Word64
      fromTimestamp = fromIntegral
