{-# LANGUAGE NumericUnderscores #-}

import Foreign.C
import Foreign.Ptr
import Control.Monad
import CycloneDDS

main :: IO ()
main = do
  putStrLn "create participant"
  dp <- c_dds_create_participant 0 nullPtr nullPtr
  putStrLn "create topic: DDSPerfRDataOU/OneULong"
  tp <- withCString "DDSPerfRDataOU" $ \tpname -> c_dds_create_topic dp topicdesc_OneULong tpname nullPtr nullPtr
  putStrLn "create writer"
  wr <- c_dds_create_writer dp tp nullPtr nullPtr
  putStrLn "write data"
  forM_ [0..] $ \i -> do
    when (i `mod` 1000_000 == 0) $ putStrLn (show i ++ " ...")
    write wr $ OneULong i
  _ <- delete dp
  pure ()
