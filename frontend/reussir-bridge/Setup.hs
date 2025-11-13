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
import System.Info (os)
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

-- Get the library file name based on the platform
getLibraryFileName :: String -> String
getLibraryFileName name = case os of
  "mingw32" -> name <.> "dll"  -- Windows
  "darwin"  -> "lib" ++ name <.> "dylib"  -- macOS
  _         -> "lib" ++ name <.> "so"  -- Linux/Unix

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
      let libFileName = getLibraryFileName "MLIRReussirBridge"
          libDir = bdir </> "lib"
          binDir = bdir </> "bin"
      
      -- Debug: List contents of lib and bin directories
      libExists <- doesDirectoryExist libDir
      binExists <- doesDirectoryExist binDir
      when libExists $ do
        libContents <- listDirectory libDir
        notice v ("Contents of " ++ libDir ++ ":\n" ++ unlines libContents)
      when binExists $ do
        binContents <- listDirectory binDir
        notice v ("Contents of " ++ binDir ++ ":\n" ++ unlines binContents)
      
      -- Try multiple possible locations for the library
      let possiblePaths = [ libDir </> libFileName
                          , binDir </> libFileName
                          , libDir </> ("lib" ++ libFileName)
                          ]
      
      foundPath <- findFirstExisting possiblePaths
      case foundPath of
        Just path -> do
          notice v ("Found library at: " ++ path)
          pure path
        Nothing -> die' v ("Library not found. Searched in: " ++ show possiblePaths)
    ExitFailure n -> die' v ("Ninja failed (" ++ show n ++ "):\n" ++ err)

-- Helper function to find the first existing file from a list
findFirstExisting :: [FilePath] -> IO (Maybe FilePath)
findFirstExisting [] = pure Nothing
findFirstExisting (p:ps) = do
  exists <- doesFileExist p
  if exists then pure (Just p) else findFirstExisting ps

-------------------------------------------------------------------------------
-- Hooks
-------------------------------------------------------------------------------
myConfHook :: (GenericPackageDescription, HookedBuildInfo)
            -> ConfigFlags -> IO LocalBuildInfo
myConfHook (pkg, pbi) flags = do
  let v = fromFlag (configVerbosity flags)

  configureCMake v
  libPath <- buildMLIRReussirBridge v
  -- Library existence is already checked in buildMLIRReussirBridge

  root <- getProjectRoot
  bdir <- getBuildDir
  let libDir = takeDirectory libPath  -- Use the actual directory where the library was found
      hookedLib = emptyBuildInfo
        { extraLibDirs = map makeSymbolicPath [libDir]
        , extraLibs    = ["MLIRReussirBridge"]
        , includeDirs  = map makeSymbolicPath [root </> "include", bdir </> "include"]
        , ldOptions    = ["-Wl,-rpath," ++ libDir]
        }
      newPbi = (Just hookedLib, snd pbi)

  -- Step 1: Run Cabal’s default configuration
  lbi <- confHook simpleUserHooks (pkg, newPbi) flags

  -- Step 2: Force patch the library’s BuildInfo so FFI stubs also see -I/-L
  let pkgDesc = localPkgDescr lbi
      fixLib bi =
        bi { includeDirs  = includeDirs bi ++ map makeSymbolicPath [root </> "include", bdir </> "include"]
           , extraLibDirs = extraLibDirs bi ++ map makeSymbolicPath [libDir]
           , extraLibs    = extraLibs bi ++ ["MLIRReussirBridge"]
           , ldOptions    = ldOptions bi ++ ["-Wl,-rpath," ++ libDir]
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
