{-# options_ghc -fno-warn-name-shadowing #-}

module Daffy
  ( run
  ) where

import Daffy.Exception
import Daffy.Stats (Stats(..), parseStats)

import Control.Exception
import Data.Text (Text)
import System.Exit
import System.IO.Temp
import System.Process

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

-- | Run a program, returning either any exception thrown or the runtime stats
-- that the run produced.
run :: String -> [String] -> IO Stats
run path args =
  withTempFile "." "daffy" $ \temp h -> do
    let args' :: [String]
        args' = args ++ ["+RTS", "-S" ++ temp]

    (Nothing, Nothing, Nothing, ph) <-
      createProcess (proc path args')

    code :: ExitCode <-
      waitForProcess ph

    case code of
      ExitFailure code -> throwIO (DaffyExitCodeException path args' code)
      ExitSuccess -> do
        contents <- Text.hGetContents h
        case parseStats (dropLine contents) of
          Left s -> throwIO (DaffyStatsParseException contents s)
          Right stats -> pure stats

dropLine :: Text -> Text
dropLine = Text.tail . snd . Text.break (== '\n')
