{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-} -- 'cos of ddssequenceMaximum

module KeyedSeq (KeyedSeq(..)) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Control.Monad (when)
import Data.Word

import CycloneDDS.TopicDescriptor

data DDSSequence a = DDSSequence
  { ddssequenceMaximum :: Word32
  , ddssequenceLength :: Word32
  , ddssequenceBuffer :: Ptr a
  , ddssequenceRelease :: Bool
  }

-- all relevant machines use 32- or 64-bit pointers, so this'll do for now
instance Storable a => Storable (DDSSequence a) where
  sizeOf _ = 8 + 2 * sizeOf (undefined :: Ptr ())
  alignment _ = alignment (undefined :: Ptr ())
  peek ptr = DDSSequence
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 4
    <*> peekByteOff ptr 8
    <*> peekByteOff ptr (8 + sizeOf (undefined :: Ptr()))
  poke ptr (DDSSequence a b c d) = do
    pokeByteOff ptr 0 a
    pokeByteOff ptr 4 b
    pokeByteOff ptr 8 c
    pokeByteOff ptr (8 + sizeOf (undefined :: Ptr ())) d

data KeyedSeq = KeyedSeq
  { keyedseqSeq :: Word32
  , keyedseqKeyval :: Word32
  , keyedseqBaggage :: [Word8]
  } deriving (Show)

instance Storable KeyedSeq where
  sizeOf _ = 8 + sizeOf (undefined :: DDSSequence Word8)
  alignment _ = alignment (undefined :: DDSSequence Word8)
  peek ptr = do
    s <- peekByteOff ptr 0
    k <- peekByteOff ptr 4
    rawb <- peekByteOff ptr 8
    b <- peekArray (fromIntegral $ ddssequenceLength rawb) (ddssequenceBuffer rawb)
    pure $ KeyedSeq s k b
  poke ptr (KeyedSeq s k b) = do
    pokeByteOff ptr 0 s
    pokeByteOff ptr 4 k
    rawbuf <- mallocArray (length b)
    pokeArray rawbuf b
    let n = fromIntegral (length b)
    pokeByteOff ptr 8 (DDSSequence n n rawbuf True)

instance AllocatingStorable KeyedSeq where
  freeAllocatedMemory ptr = do
    rawb <- peekByteOff ptr 8 :: IO (DDSSequence Word8)
    when (ddssequenceRelease rawb) $ free (ddssequenceBuffer rawb)

instance TopicDescriptor KeyedSeq where
  topicDescriptor _ = c_KeyedSeq_desc

foreign import ccall "&KeyedSeq_desc" c_KeyedSeq_desc :: TopicDescriptorPtr KeyedSeq
