{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module CycloneDDS.Sample (
  Sample(..),
  TopicType(..)
  ) where

import Data.Proxy
import qualified Data.ByteString.Lazy as BL

class (Show a, Show (TopicKey a)) => TopicType a where
  type family TopicKey a = k | k -> a
  typeName     :: Proxy a -> String
  hasKey       :: Proxy a -> Bool
  getKey       :: a -> TopicKey a
  encodeSample :: a -> BL.ByteString
  encodeKey    :: TopicKey a -> BL.ByteString
  decodeSample :: BL.ByteString -> Either String a
  decodeKey    :: BL.ByteString -> Either String (TopicKey a)

data TopicType a => Sample a = Key (TopicKey a) | Sample a deriving (Show)
