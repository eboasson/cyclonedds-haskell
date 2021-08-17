{-# LANGUAGE NumericUnderscores #-}

import System.Environment
import System.Exit
import Text.Read
import Control.Monad

import CycloneDDS
import OneULong
import KeyedSeq
import Data.Word

tryQQQ :: Participant -> IO ()
tryQQQ dp = do
  -- write data via Sertype, read it back via C
  -- (this forces serialization, even though it is inside one domain)
  qqq <- newTopic' dp [History (KeepLast 10)] "QQQ"
  dp' <- newParticipant DefaultDomainId
  qqq' <- newTopic dp' [History (KeepLast 10)] "QQQ" :: IO (Topic OneULong)
  wrqqq <- newWriter dp qqq
  rdqqq <- newReader dp qqq
  rdqqq' <- newReader dp' qqq'
  _ <- write' wrqqq $ OneULong 3_141_592_654
  _ <- write' wrqqq $ OneULong 2_718_281_828
  takeN 2 rdqqq' >>= putStrLn . show
  takeN'' 2 rdqqq >>= putStrLn . show
  void $ delete dp'

invalidArgs, invalidCount :: IO a
invalidArgs = putStrLn "usage: pp [count]" >> exitFailure
invalidCount = putStrLn "pp: invalid count specified" >> exitFailure

countFromString :: String -> IO Word32
countFromString str =
  case readMaybe str of
    Just n  -> validate n
    Nothing -> invalidCount
  where
    validate n
      | n <= 0    = invalidCount
      | otherwise = pure n

getCount :: [String] -> IO Word32
getCount []    = pure 10
getCount [x]   = countFromString x
getCount (_:_) = invalidArgs

main :: IO ()
main = do
  maxn <- getArgs >>= getCount 
  dp <- newParticipant DefaultDomainId
  ou <- newTopic dp [] "DDSPerfRDataOU"
  ks <- newTopic dp [] "DDSPerfRDataKS"
  wrOU <- newWriter dp ou
  wrKS <- newWriter dp ks
  rdKS <- newReader dp ks
  tryQQQ dp
  forM_ [0 .. maxn] $ \i -> do
    _ <- write wrOU $ OneULong i
    let k = i `mod` 5
    _ <- write wrKS $ KeyedSeq i k [ fromIntegral ((i + v) `mod` 256) | v <- [0 .. 7] ]
    when (i `mod` 1000_000 == 0) $ do
      xs <- takeN 10 rdKS -- 5 keys, keep-last-1 -> at most 5
      putStrLn (show i ++ " ... " ++ show xs)
  void $ delete dp
