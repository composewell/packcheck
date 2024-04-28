module Packcheck.Hlint where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Monad (when)
import Data.Foldable (forM_)
import Data.Function ((&))
import Streamly.Unicode.String (str)
import System.FilePath (takeDirectory, takeFileName)

import qualified Streamly.Coreutils.FileTest as CU
import qualified Streamly.Coreutils.Which as CU
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.System.Command as Cmd

import Packcheck.Common

--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------

data HlintConfig =
    HlintConfig
        { env_HLINT_OPTIONS :: String
        , env_HLINT_VERSION :: String
        , env_HLINT_PATH :: String
        , env_HLINT_URL_PREFIX :: String
        , env_HLINT_TARGETS :: [String]
        }
    deriving (Show)

--------------------------------------------------------------------------------
-- Download hlint
--------------------------------------------------------------------------------

downloadHlint :: HlintConfig -> IO ()
downloadHlint HlintConfig{..} =
    downloadTool
        $ DownloadTool
              { dtDownloadUrl =
                    oneLineUnspaced
                        [str|#{env_HLINT_URL_PREFIX}
                             /download
                             /v#{env_HLINT_VERSION}
                             /#{toolId}-#{arch}-#{os}#{archiveExt}|]
              , dtExePathInArchive = [str|#{toolId}/#{toolExe}|]
              , dtInstallDir = takeDirectory env_HLINT_PATH
              , dtExeName = toolExe
              }

    where
    toolName = takeFileName env_HLINT_PATH
    toolId = [str|#{toolName}-#{env_HLINT_VERSION}|]
    toolExe = [str|#{toolName}#{binExt}|]
    arch =
        case sysArch of
            X86_64 -> "x86_64"
            Arm_64 -> die "hlint does not support Arm architecture"
    os =
        case sysOs of
            Linux -> "linux"
            Darwin -> "osx"
            Windows -> "windows"

--------------------------------------------------------------------------------
-- Run hlint
--------------------------------------------------------------------------------

unsafeRunHlint :: HlintConfig -> IO ()
unsafeRunHlint HlintConfig{..} = do
    step "Running hlint ..."
    hlintIgnoreExists <- CU.test ".hlint.ignore" CU.isExisting
    if hlintIgnoreExists
    then do
        contents <- readFile ".hlint.ignore"
        (validEntries, invalidEntries) <-
            Stream.fromList contents
                & Stream.foldMany (Fold.takeEndBy_ (== '\n') Fold.toList)
                & Stream.filter (not . null)
                & Stream.mapM
                      (\entry -> do
                            entryExists <- CU.test entry CU.isExisting
                            pure (entryExists, entry))
                & Stream.fold
                      (Fold.tee
                           (Fold.filter fst (Fold.lmap snd Fold.toList))
                           (Fold.filter (not . fst) (Fold.lmap snd Fold.toList)))
        when (not (null invalidEntries)) $ do
            putStrLn "WARNING: The following files don't exist but are mentioned in your .hlint.ignore file."
            putStrLn $ unlines invalidEntries
            forM_ env_HLINT_TARGETS $ \target -> do
                files <-
                    filter (not . null) . lines
                        <$> verbose Cmd.toString [str|find #{target} -name "*.hs"|]
                forM_ files $ \file -> do
                    when (not (file `elem` validEntries)) $ do
                        sh [str|hlint #{env_HLINT_OPTIONS} #{file}|]
    else
        forM_ env_HLINT_TARGETS $ \target ->
            sh [str|hlint #{env_HLINT_OPTIONS} #{target}|]

ensureHlint :: HlintConfig -> IO ()
ensureHlint conf@(HlintConfig{..}) = do
    mHlintPath <- CU.which "hlint"
    case mHlintPath of
        Just hlintPath ->
            echo [str|WARNING! Using hlint in PATH at #{hlintPath}|]
        Nothing -> do
            hlintPathExists <- CU.test env_HLINT_PATH CU.isExisting
            if hlintPathExists
            then die [str|#{env_HLINT_PATH} already exists, not overwriting.|]
            else do
                echo "Hlint does not exist. Downloading hlint."
                downloadHlint conf

runHlint :: HlintConfig -> IO ()
runHlint conf = do
    ensureHlint conf
    unsafeRunHlint conf
