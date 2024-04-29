{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import qualified Packcheck.Hlint as Hlint
import qualified Packcheck.Ghcup as Ghcup

import System.Console.CmdArgs

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

data Cli
    = Hlint
          { hlint_version :: String
          , install_path :: String
          , url_prefix :: String
          , hlint_options :: String
          , hlint_targets :: String -- Space seperated hlint targets
          }
    | Ghcup
          { url_prefix :: String
          , ghcup_version :: String
          , install_path :: FilePath
          , tool_name :: String
          , tool_install_options :: String
          , tool_version :: String
          }
    deriving (Show, Data, Typeable)

ghcupMode :: Cli
ghcupMode =
    Ghcup
        { url_prefix =
            def &= help "ghcup url prefix to install from."
                &= typ "URL"
                &= groupname "Compile-time"
        , ghcup_version =
            def &= help "ghcup version to install."
                &= typ "STRING"
                &= groupname "Compile-time"
        , install_path =
            def &= help "path to install/lookup ghcup at."
                &= typFile
                &= groupname "Compile-time"
        , tool_name =
            def &= help "tool to install via ghcup."
                &= typ "STRING"
                &= groupname "Run-time"
        , tool_install_options =
            def &= help "hlint_options for the tool installation via ghcup."
                &= typ "STRING"
                &= groupname "Run-time"
        , tool_version =
            def &= help "tool version to install via ghcup."
                &= typ "STRING"
                &= groupname "Run-time"
        } &= help "run ghcup"

hlintMode :: Cli
hlintMode =
    Hlint
        { hlint_options =
            def &= help "hlint_options passed to hlint."
                &= typ "STRING"
                &= groupname "Run-time"
        , hlint_version =
            def &= typ "STRING"
                &= help "hlint version."
                &= groupname "Compile-time"
        , install_path =
            def &= typDir
                &= help "path to install/lookup hlint at."
                &= groupname "Compile-time"
        , url_prefix =
            def &= typ "URL"
                &= help "url to download hlint from."
                &= groupname "Compile-time"
        , hlint_targets =
            def &= typ "STRING"
                &= help "hlint hlint_targets."
                &= groupname "Run-time"
        } &= help "run hlint"

cliModes :: Cli
cliModes =
    modes [hlintMode, ghcupMode]
        &= program "packcheck"

main :: IO ()
main = do
    opts <- cmdArgs cliModes
    case opts of
        Hlint {..} ->
            Hlint.runHlint
                $ Hlint.HlintConfig
                      { Hlint.env_HLINT_OPTIONS = hlint_options
                      , Hlint.env_HLINT_VERSION = hlint_version
                      , Hlint.env_HLINT_PATH = install_path
                      , Hlint.env_HLINT_URL_PREFIX = url_prefix
                      , Hlint.env_HLINT_TARGETS = hlint_targets
                      }
        Ghcup {..} ->
            Ghcup.runGhcupWith
                  (Ghcup.GhcupConfig
                       { Ghcup.env_GHCUP_URL_PREFIX = url_prefix
                       , Ghcup.env_GHCUP_VERSION = ghcup_version
                       , Ghcup.env_GHCUP_PATH = install_path
                       })
                  tool_name
                  tool_install_options
                  tool_version
