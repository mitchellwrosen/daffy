module Daffy
  ( DaffyException
  , run
  ) where

import Daffy.Stats (Stats(..), parseStats)

import Control.Exception
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Typeable (Typeable)
import System.Exit
import System.IO.Temp
import System.Process

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8

data DaffyException
  = DaffyParseException String
  deriving (Show, Typeable)

instance Exception DaffyException

-- | Run a program, returning either any exception thrown or the runtime stats
-- that the run produced.
run :: String -> [String] -> IO (Either SomeException Stats)
run prog args =
  withTempFile "." "daffy" $ \temp h -> do
    let spec :: CreateProcess
        spec = proc prog (args ++ ["+RTS", "-S" ++ temp])

    let action :: IO (Either SomeException Stats)
        action = do
          (Nothing, Nothing, Nothing, ph) <-
            createProcess spec

          code :: ExitCode <-
            waitForProcess ph

          case code of
            ExitFailure _ -> pure (Left (toException code))
            ExitSuccess -> do
              first (toException . DaffyParseException) . parseStats . dropLine
                <$> ByteString.hGetContents h

    action `catch` \ex -> pure (Left ex)

dropLine :: ByteString -> ByteString
dropLine = ByteString.tail . Char8.dropWhile (/= '\n')
