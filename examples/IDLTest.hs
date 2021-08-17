{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad
import CycloneDDS
import CycloneDDS.TopicDescriptor

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Data.Word
import qualified Data.ByteString.Lazy as BL


data TypeA = TypeA
  { tab_c0 :: Word16
  , tab_c1 :: Word16
  , tab_i :: String
  } deriving (Show)

sizeof_ptr :: Int
sizeof_ptr = sizeOf (undefined :: Ptr ())

instance Storable TypeA where
  sizeOf _ = 2 * sizeof_ptr
  alignment _ = sizeof_ptr
  peek ptr = do
    a <- peekByteOff ptr 0
    b <- peekByteOff ptr 2
    c <- peekByteOff ptr sizeof_ptr >>= peekCString
    pure $ TypeA a b c
  poke ptr (TypeA a b c) = do
    pokeByteOff ptr 0 a
    pokeByteOff ptr 2 b
    newCString c >>= pokeByteOff ptr sizeof_ptr

instance AllocatingStorable TypeA where
  freeAllocatedMemory ptr = peekByteOff ptr sizeof_ptr >>= free

instance TopicDescriptor TypeA where
  topicDescriptor _ = c_fuzzymod_TypeA_desc

foreign import ccall "&fuzzymod_TypeA_desc" c_fuzzymod_TypeA_desc :: TopicDescriptorPtr TypeA

data Blob = Blob [Word8] deriving (Show)
data BlobKey = BlobKey Word16 Word16 String deriving (Show)
instance TopicType Blob where
  type TopicKey Blob = BlobKey
  typeName = const "fuzzymod::TypeA"
  getKey = undefined
  hasKey = const True
  encodeKey = undefined
  decodeKey = undefined
  decodeSample = undefined
  encodeSample (Blob xs) = BL.pack xs

main :: IO ()
main = do
  dpA <- newParticipant $ DomainId 0
  tpA <- newTopic dpA [] "A" :: IO (Topic TypeA)
  rdA <- newReader dpA tpA
  dpB <- newParticipant $ DomainId 0
  tpB <- newTopic' dpB [] "A" :: IO (Topic Blob)
  wrB <- newWriter dpB tpB
  _ <- write' wrB $ Blob [0x00,0x01,0x00,0x00,0x01,0x00,0x01,0x00,0x07,0x00,0x00,0x00,0x68,0x65,0x6c,0x6c,0x6f,0x21,0x00]
  takeN 1 rdA >>= putStrLn . show
  void $ delete dpA
  void $ delete dpB
