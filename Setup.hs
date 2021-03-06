{-# LANGUAGE RecordWildCards #-}
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.GHC
import Distribution.System
import Distribution.PackageDescription
import System.Environment
import System.Exit
import System.Process
import Control.Monad

--main = defaultMainWithHooks $ autoconfUserHooks { postBuild = addRpath }
main = defaultMainWithHooks $ simpleUserHooks { postBuild = addRpath }

addRpath args bf pkg lbi 
  = case buildOS of
      OSX -> addRPathOSX bf lbi
      _   -> return ()

addRPathOSX bf lbi = do
  let unitId = localUnitId lbi
      ghcVer = (filter (/= '-') . showCompilerId . compiler) lbi
      file = buildDir lbi ++ "/lib" ++ getHSLibraryName unitId ++ "-" ++ ghcVer ++ ".dylib"
  --osplHome <- getEnv "OSPL_HOME"
  cddsHome <- pure "/Users/erik/C/cdds/CC/install"
  --spliceTarget <- liftM (maybe "" ('/':)) $ lookupEnv "SPLICE_TARGET"
  --let osplLibs = osplHome ++ "/lib" ++ spliceTarget
  let cddsLibs = cddsHome ++ "/lib"
  putStrLn $ "Post build: lib = " ++ file
  putStrLn $ "Post build: dir = " ++ cddsLibs
  executeShellCommand $ "install_name_tool -add_rpath " ++ cddsLibs ++ " " ++ file

executeShellCommand cmd = putStrLn ("EXEC: " ++ cmd) >> system cmd >>= check
  where
    check (ExitSuccess) = return ()
    check (ExitFailure n) = error $ "cmd: " ++ cmd ++ " failure code " ++ show n
