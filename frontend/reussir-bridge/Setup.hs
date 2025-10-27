{-# LANGUAGE CPP #-}

import Control.Monad (unless, when)
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

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { confHook = myConfHook,
        buildHook = myBuildHook,
        cleanHook = myCleanHook
      }

-- Get the project root directory (two levels up from this package)
getProjectRoot :: IO FilePath
getProjectRoot = do
  currentDir <- getCurrentDirectory
  -- frontend/reussir-bridge -> frontend -> project root
  return $ takeDirectory (takeDirectory currentDir)

-- Get the build directory path
getBuildDir :: IO FilePath
getBuildDir = do
  projectRoot <- getProjectRoot
  return $ projectRoot </> "build"

-- Check if CMake build directory exists and is configured
isCMakeConfigured :: FilePath -> IO Bool
isCMakeConfigured buildDir = do
  exists <- doesDirectoryExist buildDir
  if exists
    then doesFileExist (buildDir </> "CMakeCache.txt")
    else return False

-- Configure CMake if not already done
configureCMake :: Verbosity -> IO ()
configureCMake verbosity = do
  projectRoot <- getProjectRoot
  buildDir <- getBuildDir
  configured <- isCMakeConfigured buildDir

  unless configured $ do
    notice verbosity "Configuring CMake project..."
    createDirectoryIfMissing True buildDir

    let cmakeArgs =
          [ "-GNinja",
            "-DCMAKE_BUILD_TYPE=Release",
            "-DREUSSIR_ENABLE_TESTS=OFF",
            projectRoot
          ]
        cmakeProc = (proc "cmake" cmakeArgs) {cwd = Just buildDir}

    (exitCode, _, stderr) <- readCreateProcessWithExitCode cmakeProc ""
    case exitCode of
      ExitSuccess -> notice verbosity "CMake configuration successful"
      ExitFailure code -> do
        die' verbosity $ "CMake configuration failed with code " ++ show code ++ "\n" ++ stderr

-- Build the MLIRReussirBridge library
buildMLIRReussirBridge :: Verbosity -> IO FilePath
buildMLIRReussirBridge verbosity = do
  buildDir <- getBuildDir

  notice verbosity "Building MLIRReussirBridge library..."

  let ninjaArgs = ["MLIRReussirBridge"]
      ninjaProc = (proc "ninja" ninjaArgs) {cwd = Just buildDir}

  (exitCode, stdout, stderr) <- readCreateProcessWithExitCode ninjaProc ""
  case exitCode of
    ExitSuccess -> do
      notice verbosity "MLIRReussirBridge build successful"
      -- Return the path to the built library
      return $ buildDir </> "lib" </> "libMLIRReussirBridge.so"
    ExitFailure code -> do
      die' verbosity $ "Ninja build failed with code " ++ show code ++ "\n" ++ stdout ++ "\n" ++ stderr

-- Custom configuration hook
myConfHook :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
myConfHook (pkg_descr, pbi) flags = do
  let verbosity = fromFlag $ configVerbosity flags

  -- Configure CMake
  configureCMake verbosity

  -- Call the default configuration
  lbi <- confHook simpleUserHooks (pkg_descr, pbi) flags

  -- Get build directory
  buildDir <- getBuildDir
  projectRoot <- getProjectRoot

  -- Add extra library directories and libraries
  let libDirs = map makeSymbolicPath [buildDir </> "lib"]
      libs = ["MLIRReussirBridge"]
      includeDirs =
        map
          makeSymbolicPath
          [ projectRoot </> "include",
            buildDir </> "include"
          ]
      -- Add rpath so the library can be found at runtime
      ldOpts = ["-Wl,-rpath," ++ (buildDir </> "lib")]

  -- Update the local build info with our custom settings
  let updateLib lib =
        lib
          { libBuildInfo =
              (libBuildInfo lib)
                { extraLibDirs = extraLibDirs (libBuildInfo lib) ++ libDirs,
                  extraLibs = extraLibs (libBuildInfo lib) ++ libs,
                  includeDirs = includeDirs ++ Distribution.PackageDescription.includeDirs (libBuildInfo lib),
                  ldOptions = ldOptions (libBuildInfo lib) ++ ldOpts
                }
          }

  let updatedPkgDescr = case library (localPkgDescr lbi) of
        Nothing -> localPkgDescr lbi
        Just lib -> (localPkgDescr lbi) {library = Just (updateLib lib)}

  return lbi {localPkgDescr = updatedPkgDescr}

-- Custom build hook
myBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
myBuildHook pkg_descr lbi hooks flags = do
  let verbosity = fromFlag $ buildVerbosity flags

  -- Build the C++ library first
  libPath <- buildMLIRReussirBridge verbosity

  -- Verify the library was built
  libExists <- doesFileExist libPath
  unless libExists $ do
    die' verbosity $ "Failed to build library: " ++ libPath

  notice verbosity $ "MLIRReussirBridge library available at: " ++ libPath

  -- Now build the Haskell package
  buildHook simpleUserHooks pkg_descr lbi hooks flags

-- Custom clean hook
myCleanHook :: PackageDescription -> () -> UserHooks -> CleanFlags -> IO ()
myCleanHook pkg_descr unit hooks flags = do
  let verbosity = fromFlag $ cleanVerbosity flags

  notice verbosity "Cleaning MLIRReussirBridge (skipping - managed by CMake)..."

  -- Call the default clean
  cleanHook simpleUserHooks pkg_descr unit hooks flags
