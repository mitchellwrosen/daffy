module Daffy
  ( run
  ) where

import Daffy.Exception
import Daffy.Stats (Stats(..), parseStats)

import Control.Exception
import Data.ByteString (ByteString)
import System.Exit
import System.IO.Temp
import System.Process

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8

-- | Run a program, returning either any exception thrown or the runtime stats
-- that the run produced.
run :: String -> [String] -> IO Stats
run prog args =
  withTempFile "." "daffy" $ \temp h -> do
    let spec :: CreateProcess
        spec = proc prog (args ++ ["+RTS", "-S" ++ temp])

    (Nothing, Nothing, Nothing, ph) <-
      createProcess spec

    code :: ExitCode <-
      waitForProcess ph

    case code of
      ExitFailure _ -> throwIO code
      ExitSuccess -> do
        contents <- ByteString.hGetContents h
        case parseStats (dropLine contents) of
          Left s -> throwIO (DaffyStatsParseException s)
          Right stats -> pure stats


dropLine :: ByteString -> ByteString
dropLine = ByteString.tail . Char8.dropWhile (/= '\n')
