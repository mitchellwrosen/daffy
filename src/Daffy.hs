module Daffy
  ( run
  ) where

import Control.Exception
import Data.Text (Text)
import System.Exit
import System.IO
import System.IO.Temp
import System.Process

run :: String -> [String] -> IO (Either SomeException [(Text, Text)])
run prog args =
  withTempFile "." "daffy" $ \temp h -> do
    let spec :: CreateProcess
        spec = proc prog (args ++ ["+RTS", "-t" ++ temp, "--machine-readable"])

    let action :: IO (Either SomeException [(Text, Text)])
        action = do
          (Nothing, Nothing, Nothing, ph) <-
            createProcess spec

          code :: ExitCode <-
            waitForProcess ph

          case code of
            ExitFailure _ -> pure (Left (toException code))
            ExitSuccess -> do
              contents <- hGetContents h
              let !_ = length contents
              pure (Right (read (unlines (tail (lines contents)))))

    action `catch` \ex -> pure (Left ex)
