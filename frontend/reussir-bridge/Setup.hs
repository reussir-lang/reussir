import Control.Monad (unless)
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Utils.Path (makeSymbolicPath)
import Distribution.Verbosity
import System.Directory
import System.Environment (lookupEnv)
import System.Exit
import System.FilePath
import System.Info (os)
import System.Process

-------------------------------------------------------------------------------
-- Entry point (FIXED)
-------------------------------------------------------------------------------
main :: IO ()
main =
    defaultMainWithHooks
        simpleUserHooks
            { confHook = myConfHook
            , buildHook = myBuildHook
            , cleanHook = myCleanHook
            }

-------------------------------------------------------------------------------
-- Path helpers
-------------------------------------------------------------------------------
getProjectRoot :: IO FilePath
getProjectRoot = do
    mRoot <- lookupEnv "REUSSIR_PROJECT_ROOT"
    case mRoot of
        Just root -> return root
        Nothing -> getCurrentDirectory >>= findProjectRoot
  where
    findProjectRoot :: FilePath -> IO FilePath
    findProjectRoot dir = do
        let marker = dir </> "cabal.project"
        exists <- doesFileExist marker
        if exists
            then return dir
            else do
                let parent = takeDirectory dir
                if parent == dir
                    then fail "Could not find project root (cabal.project not found). Please set REUSSIR_PROJECT_ROOT."
                    else findProjectRoot parent

getBuildDir :: IO FilePath
getBuildDir = (</> "build") <$> getProjectRoot

-------------------------------------------------------------------------------
-- Platform detection
-------------------------------------------------------------------------------
isWindows :: Bool
isWindows = os == "mingw32"

isDarwin :: Bool
isDarwin = os == "darwin"

getLibDir :: FilePath -> FilePath
getLibDir bdir = if isWindows then bdir </> "bin" else bdir </> "lib"

getSharedLibName :: String -> String
getSharedLibName name
    | isWindows = "lib" ++ name ++ ".dll"
    | isDarwin  = "lib" ++ name ++ ".dylib"
    | otherwise = "lib" ++ name ++ ".so"

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
        let args =
                [ "-GNinja"
                , "-DCMAKE_BUILD_TYPE=Release"
                , "-DREUSSIR_ENABLE_TESTS=OFF"
                , root
                ]
        (code, _out, err) <- readCreateProcessWithExitCode (proc "cmake" args){cwd = Just bdir} ""
        case code of
            ExitSuccess -> notice v "CMake configuration successful"
            ExitFailure n -> die' v ("CMake failed (" ++ show n ++ "):\n" ++ err)

buildMLIRReussirBridge :: Verbosity -> IO FilePath
buildMLIRReussirBridge v = do
    bdir <- getBuildDir
    notice v "Building MLIRReussirBridge..."
    (code, out, err) <- readCreateProcessWithExitCode (proc "ninja" ["MLIRReussirBridge"]){cwd = Just bdir} ""
    unless (null out) (notice v ("Ninja stdout:\n" ++ out))
    unless (null err) (notice v ("Ninja stderr:\n" ++ err))
    case code of
        ExitSuccess -> do
            notice v "MLIRReussirBridge build successful"
            pure (getLibDir bdir </> getSharedLibName "MLIRReussirBridge")
        ExitFailure n -> die' v ("Ninja failed (" ++ show n ++ "):\n" ++ err)

-------------------------------------------------------------------------------
-- Hooks
-------------------------------------------------------------------------------
myConfHook ::
    (GenericPackageDescription, HookedBuildInfo) ->
    ConfigFlags ->
    IO LocalBuildInfo
myConfHook (pkg, pbi) flags = do
    let v = fromFlag (configVerbosity flags)

    configureCMake v
    libPath <- buildMLIRReussirBridge v
    exists <- doesFileExist libPath
    unless exists $ die' v ("Library not found: " ++ libPath)

    root <- getProjectRoot
    bdir <- getBuildDir
    let libDir = getLibDir bdir
        rpathOpts
            | isWindows = []
            | isDarwin  = ["-Wl,-rpath,@loader_path/../lib", "-Wl,-rpath," ++ libDir]
            | otherwise = ["-Wl,-rpath," ++ libDir]
        hookedLib =
            emptyBuildInfo
                { extraLibDirs = map makeSymbolicPath [libDir]
                , extraLibs = ["MLIRReussirBridge"]
                , includeDirs = map makeSymbolicPath [root </> "include", bdir </> "include"]
                , ldOptions = rpathOpts
                }
        newPbi = (Just hookedLib, snd pbi)

    -- Step 1: Run Cabal’s default configuration
    lbi <- confHook simpleUserHooks (pkg, newPbi) flags

    -- Step 2: Force patch the library's BuildInfo so FFI stubs also see -I/-L
    let pkgDesc = localPkgDescr lbi
        fixLib bi =
            bi
                { includeDirs = includeDirs bi ++ map makeSymbolicPath [root </> "include", bdir </> "include"]
                , extraLibDirs = extraLibDirs bi ++ map makeSymbolicPath [libDir]
                , extraLibs = extraLibs bi ++ ["MLIRReussirBridge"]
                , ldOptions = ldOptions bi ++ rpathOpts
                }
        pkgDesc' = updatePackageDescription (Just (fixLib emptyBuildInfo), []) pkgDesc
    pure lbi{localPkgDescr = pkgDesc'}

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
