module Packcheck.Common where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Concurrent (threadDelay)
import Control.Exception (Exception(..), SomeException)
import Control.Monad.Catch (catch)
import System.FilePath (takeFileName)
import Streamly.System.Process (ProcessFailure)
import Streamly.Unicode.String (str)

import qualified Streamly.Internal.System.Command as Cmd
import qualified System.IO.Temp as Temp

--------------------------------------------------------------------------------
-- Os specific constants
--------------------------------------------------------------------------------

data SysOs = Darwin | Linux | Windows
data SysArch = X86_64 | Arm_64

archiveExt, binExt :: String
sysOs :: SysOs
sysArch :: SysArch
#if defined(mingw32_HOST_OS)
archiveExt = ".zip"
sysOs = Windows
binExt = ".exe"
#elif defined(darwin_HOST_OS)
archiveExt = ".tar.gz"
sysOs = Darwin
binExt = ""
#elif defined(linux_HOST_OS)
archiveExt = ".tar.gz"
sysOs = Linux
binExt = ""
#endif
#if defined(x86_64_HOST_ARCH)
sysArch = X86_64
#elif defined(arm_HOST_ARCH)
sysArch = Arm_64
#endif

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

oneLine :: String -> String
oneLine = unwords . filter (not . null) . words

oneLineUnspaced :: String -> String
oneLineUnspaced = concat . filter (not . null) . words

echo :: String -> IO ()
echo = putStrLn

die :: String -> a
die = error

step :: String -> IO ()
step title = do
    echo ""
    echo "--------------------------------------------------"
    echo title
    echo "--------------------------------------------------"

verbose :: (String -> IO a) -> String -> IO a
verbose runner cmd = do
    putStrLn $ ">> " ++ cmd
    runner cmd

sh :: String -> IO ()
sh cmd = do
    catch
        (verbose Cmd.toStdout cmd)
        (\(e :: ProcessFailure) ->
             let eStr = displayException e
              in die [str|Command [#{cmd}] failed with exception #{eStr}.|])

tenPow6 :: Int
tenPow6 = 1000000

retry :: IO a -> IO a
retry action =
    catch1 action (catch1 (withDelay 2 action) (withDelay 10 action))

    where

    catch1 act next = catch act (\(_ :: SomeException) -> next)

    withDelay nSec act = do
        threadDelay (nSec * tenPow6)
        act

-------------------------------------------------------------------------------
-- Install a generic tool
--------------------------------------------------------------------------------

data DownloadTool =
    DownloadTool
        { dtDownloadUrl :: String
        , dtInstallDir :: FilePath
        , dtExeName :: String
        , dtExePathInArchive :: FilePath
        }

downloadToolSimple :: DownloadTool -> IO ()
downloadToolSimple DownloadTool{..} = do
    step [str|Downloading #{dtDownloadUrl}|]
    Temp.withSystemTempDirectory "temp" $ \tmpDir -> do
        let downloadTo = [str|#{tmpDir}/binary#{binExt}|]
        let curlCmd =
                oneLine
                    [str|curl
                         --fail --progress-bar --location
                         -o#{downloadTo} #{dtDownloadUrl}|]
        retry (sh curlCmd)
        sh [str|mkdir -p #{dtInstallDir}|]
        sh [str|mv #{tmpDir}/#{dtExePathInArchive} #{dtInstallDir}/#{dtExeName}|]
        sh [str|chmod +x #{dtInstallDir}/#{dtExeName}|]

downloadTool :: DownloadTool -> IO ()
downloadTool DownloadTool{..} = do
    step [str|Downloading #{dtDownloadUrl}|]
    Temp.withSystemTempDirectory "temp" $ \tmpDir -> do
        let archiveName = [str|tempArchive#{archiveExt}|]
            downloadTo = [str|#{tmpDir}/#{archiveName}|]
            exeNameInArchive = takeFileName dtExePathInArchive
        let curlCmd =
                oneLine
                    [str|curl
                         --fail --progress-bar --location
                         -o#{downloadTo} #{dtDownloadUrl}|]
#if defined(mingw32_HOST_OS)
        let untarCmd =
                oneLine
                    [str|7z x
                         -y #{downloadTo}
                         -o#{tmpDir}
                         #{dtExePathInArchive} > /dev/null|]
#elif defined(darwin_HOST_OS)
        let untarCmd =
                oneLine
                    [str|tar
                         -xzvf #{downloadTo}
                         -C#{tmpDir}
                         --include '*/#{exeNameInArchive}'|]
#elif defined(linux_HOST_OS)
        let untarCmd =
                oneLine
                    [str|tar
                         -xzvf #{tmpDir}/#{archiveName}
                         -C#{tmpDir}
                         --wildcards '*/#{exeNameInArchive}'|]
#endif
        retry (sh curlCmd)
        sh [str|mkdir -p #{dtInstallDir}|]
        sh untarCmd
        sh [str|mv #{tmpDir}/#{dtExePathInArchive} #{dtInstallDir}/#{dtExeName}|]
        sh [str|chmod +x #{dtInstallDir}/#{dtExeName}|]
