import Control.Monad (unless)
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Utils.Path (makeSymbolicPath)
import Distribution.Verbosity
import System.Directory
import System.Exit
import System.FilePath
import System.Process

-------------------------------------------------------------------------------
-- Entry point (FIXED)
-------------------------------------------------------------------------------
main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { confHook  = myConfHook
  , buildHook = myBuildHook
  , cleanHook = myCleanHook
  }

-------------------------------------------------------------------------------
-- Path helpers
-------------------------------------------------------------------------------
getProjectRoot, getBuildDir :: IO FilePath
getProjectRoot = takeDirectory . takeDirectory <$> getCurrentDirectory
getBuildDir    = (</> "build") <$> getProjectRoot

-------------------------------------------------------------------------------
-- CMake helpers
-------------------------------------------------------------------------------
isCMakeConfigured :: FilePath -> IO Bool
isCMakeConfigured dir = do
  exists <- doesDirectoryExist dir
  if exists then doesFileExist (dir </> "CMakeCache.txt") else pure False

configureCMake :: Verbosity -> IO ()
configureCMake v = do
  root <- getProjectRoot
  bdir <- getBuildDir
  conf <- isCMakeConfigured bdir
  unless conf $ do
    notice v "Configuring CMake project..."
    createDirectoryIfMissing True bdir
    let args = [ "-GNinja"
               , "-DCMAKE_BUILD_TYPE=Release"
               , "-DREUSSIR_ENABLE_TESTS=OFF"
               , root ]
    (code, _out, err) <- readCreateProcessWithExitCode (proc "cmake" args){cwd=Just bdir} ""
    case code of
      ExitSuccess   -> notice v "CMake configuration successful"
      ExitFailure n -> die' v ("CMake failed (" ++ show n ++ "):\n" ++ err)

buildMLIRReussirBridge :: Verbosity -> IO FilePath
buildMLIRReussirBridge v = do
  bdir <- getBuildDir
  notice v "Building MLIRReussirBridge..."
  (code, out, err) <- readCreateProcessWithExitCode (proc "ninja" ["MLIRReussirBridge"]){cwd=Just bdir} ""
  unless (null out) (notice v ("Ninja stdout:\n" ++ out))
  unless (null err) (notice v ("Ninja stderr:\n" ++ err))
  case code of
    ExitSuccess -> do
      notice v "MLIRReussirBridge build successful"
      pure (bdir </> "lib" </> "libMLIRReussirBridge.so")
    ExitFailure n -> die' v ("Ninja failed (" ++ show n ++ "):\n" ++ err)

-------------------------------------------------------------------------------
-- Hooks
-------------------------------------------------------------------------------
myConfHook :: (GenericPackageDescription, HookedBuildInfo)
            -> ConfigFlags -> IO LocalBuildInfo
myConfHook (pkg, pbi) flags = do
  let v = fromFlag (configVerbosity flags)

  configureCMake v
  libPath <- buildMLIRReussirBridge v
  exists <- doesFileExist libPath
  unless exists $ die' v ("Library not found: " ++ libPath)

  root <- getProjectRoot
  bdir <- getBuildDir
  let hookedLib = emptyBuildInfo
        { extraLibDirs = map makeSymbolicPath [bdir </> "lib"]
        , extraLibs    = ["MLIRReussirBridge"]
        , includeDirs  = map makeSymbolicPath [root </> "include", bdir </> "include"]
        , ldOptions    = ["-Wl,-rpath," ++ (bdir </> "lib")]
        }
      newPbi = (Just hookedLib, snd pbi)

  -- Step 1: Run Cabal’s default configuration
  lbi <- confHook simpleUserHooks (pkg, newPbi) flags

  -- Step 2: Force patch the library’s BuildInfo so FFI stubs also see -I/-L
  let pkgDesc = localPkgDescr lbi
      fixLib bi =
        bi { includeDirs  = includeDirs bi ++ map makeSymbolicPath [root </> "include", bdir </> "include"]
           , extraLibDirs = extraLibDirs bi ++ map makeSymbolicPath [bdir </> "lib"]
           , extraLibs    = extraLibs bi ++ ["MLIRReussirBridge"]
           , ldOptions    = ldOptions bi ++ ["-Wl,-rpath," ++ (bdir </> "lib")]
           }
      pkgDesc' = updatePackageDescription (Just (fixLib emptyBuildInfo), []) pkgDesc
  pure lbi { localPkgDescr = pkgDesc' }

-------------------------------------------------------------------------------
myBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
myBuildHook pkg lbi hooks flags = do
  let v = fromFlag (buildVerbosity flags)
  bdir <- getBuildDir
  configured <- isCMakeConfigured bdir
  unless configured (configureCMake v)
  _ <- buildMLIRReussirBridge v
  buildHook simpleUserHooks pkg lbi hooks flags

-------------------------------------------------------------------------------
myCleanHook :: PackageDescription -> () -> UserHooks -> CleanFlags -> IO ()
myCleanHook pkg () hooks flags = do
  let v = fromFlag (cleanVerbosity flags)
  notice v "Cleaning MLIRReussirBridge (skipping actual CMake clean)."
  cleanHook simpleUserHooks pkg () hooks flags
