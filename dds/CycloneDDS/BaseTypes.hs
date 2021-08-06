{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CycloneDDS.BaseTypes (
  Handle(..),
  InstanceHandle(..)) where

import Foreign.Storable
import Data.Int
import Data.Word

newtype Handle a = Handle { unHandle :: Int32 }
  deriving (Eq, Ord, Show, Read)

newtype InstanceHandle = InstanceHandle { unInstanceHandle :: Word64 }
  deriving (Eq, Ord, Show, Read, Storable)
