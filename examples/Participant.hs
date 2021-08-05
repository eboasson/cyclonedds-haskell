{-# LANGUAGE NumericUnderscores #-}

import Control.Monad
import CycloneDDS
import OneULong

main :: IO ()
main = do
  dp <- newParticipant DefaultDomainId
  tp <- newTopic dp "DDSPerfRDataOU"
  wr <- newWriter dp tp
  putStrLn "write data"
  forM_ [0..] $ \i -> do
    when (i `mod` 1000_000 == 0) $ putStrLn (show i ++ " ...")
    write wr $ OneULong i
  _ <- delete dp
  pure ()
