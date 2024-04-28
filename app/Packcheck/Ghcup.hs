module Packcheck.Ghcup where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import System.FilePath (takeDirectory, takeFileName)
import Streamly.Unicode.String (str)

import qualified Streamly.Coreutils.FileTest as CU
import qualified Streamly.Coreutils.Which as CU

import Packcheck.Common

--------------------------------------------------------------------------------
-- Installation
--------------------------------------------------------------------------------

data GhcupConfig =
    GhcupConfig
        { env_GHCUP_URL_PREFIX :: String
        , env_GHCUP_VERSION :: String
        , env_GHCUP_PATH :: String
        }
    deriving (Show)

downloadGhcup :: GhcupConfig -> IO ()
downloadGhcup GhcupConfig{..} =
    downloadToolSimple
        $ DownloadTool
              { dtDownloadUrl =
                    oneLineUnspaced
                        [str|#{env_GHCUP_URL_PREFIX}
                             /#{env_GHCUP_VERSION}
                             /#{arch}-#{os}-#{toolExe}-#{env_GHCUP_VERSION}|]
              , dtExePathInArchive = toolExe
              , dtInstallDir = takeDirectory env_GHCUP_PATH
              , dtExeName = toolExe
              }

    where
    toolExe = takeFileName env_GHCUP_PATH
    arch =
        case sysArch of
            X86_64 -> "x86_64"
            Arm_64 -> "aarch64"
    os =
        case sysOs of
            Linux -> "linux"
            Darwin -> "apple-darwin"
            Windows -> die "ghcup does not support windows"

ensureGhcup :: GhcupConfig -> IO ()
ensureGhcup conf@(GhcupConfig{..}) = do
    mGhcupPath <- CU.which "ghcup"
    case mGhcupPath of
        Just ghcupPath ->
            echo [str|Using ghcup in PATH at #{ghcupPath}|]
        Nothing -> do
            ghcupPathExists <- CU.test env_GHCUP_PATH CU.isExisting
            if ghcupPathExists
            then die [str|#{env_GHCUP_PATH} already exists, not overwriting.|]
            else do
                echo "ghcup does not exist. Downloading ghcup."
                downloadGhcup conf

runGhcupWith :: GhcupConfig -> String -> String -> String -> IO ()
runGhcupWith conf toolName toolInstallOptions toolVersion = do
    ensureGhcup conf
    sh [str|ghcup install #{toolName} #{toolInstallOptions} #{toolVersion}|]

runGhcup :: GhcupConfig -> String -> String -> IO ()
runGhcup conf toolName toolVersion =
    runGhcupWith conf toolName "" toolVersion
