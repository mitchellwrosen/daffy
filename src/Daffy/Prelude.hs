module Daffy.Prelude
  ( module X
  , LByteString
  ) where

import qualified Data.ByteString.Lazy

import Data.Text as X (Text)
import GHC.Generics as X (Generic)
import System.IO as X (hPutStrLn, stderr)
import System.Exit as X (exitFailure)

type LByteString
  = Data.ByteString.Lazy.ByteString
