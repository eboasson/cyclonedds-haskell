{-# LANGUAGE NumericUnderscores #-}

import Control.Monad
import CycloneDDS
import OneULong

main :: IO ()
main = do
  dp <- newParticipant DefaultDomainId
  tp <- newTopic dp "DDSPerfRDataOU"
  wr <- newWriter dp tp
  rd <- newReader dp tp
  forM_ [0..] $ \i -> do
    _ <- write wr $ OneULong i
    when (i `mod` 1000_000 == 0) $ do
      xs <- takeN 10 rd -- should be at most 1 with default QoS
      putStrLn (show i ++ " ... " ++ show xs)
  _ <- delete dp
  pure ()
