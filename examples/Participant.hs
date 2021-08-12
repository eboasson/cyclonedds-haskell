{-# LANGUAGE NumericUnderscores #-}

import Control.Monad
import CycloneDDS
import OneULong
import KeyedSeq

tryQQQ :: Participant -> IO ()
tryQQQ dp = do
  qqq <- newTopicQQQ dp "QQQ"
  dp' <- newParticipant DefaultDomainId
  qqq' <- newTopic dp' "QQQ" :: IO (Topic OneULong)
  wrqqq <- newWriter dp qqq
  rdqqq' <- newReader dp' qqq'
  _ <- write' wrqqq $ OneULong 3_141_592_654
  xs <- takeN 1 rdqqq'
  putStrLn (show xs)
  void $ delete dp'

main :: IO ()
main = do
  dp <- newParticipant DefaultDomainId
  ou <- newTopic dp "DDSPerfRDataOU"
  ks <- newTopic dp "DDSPerfRDataKS"
  wrOU <- newWriter dp ou
  wrKS <- newWriter dp ks
  rdKS <- newReader dp ks
  tryQQQ dp
  forM_ [0..] $ \i -> do
    _ <- write wrOU $ OneULong i
    let k = i `mod` 5
    _ <- write wrKS $ KeyedSeq i k [ fromIntegral ((i + v) `mod` 256) | v <- [0 .. 7] ]
    when (i `mod` 1000_000 == 0) $ do
      xs <- takeN 10 rdKS -- 5 keys, keep-last-1 -> at most 5
      putStrLn (show i ++ " ... " ++ show xs)
  void $ delete dp
