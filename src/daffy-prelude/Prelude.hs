module Prelude
  ( module X
  , LByteString
  ) where

import qualified Data.ByteString.Lazy

import Control.Applicative as X
import Control.Monad as X
import Control.Monad.IO.Class as X
import Control.Exception as X (Exception)
import Control.Exception.Safe as X (throw)
import Data.Either as X
import Data.Text as X (Text)
import Data.Typeable as X (Typeable)
import GHC.Generics as X (Generic)
import "base" Prelude as X
import System.IO as X (hPutStrLn, stderr)
import System.Exit as X (exitFailure)
import Text.Read as X (readMaybe)

type LByteString
  = Data.ByteString.Lazy.ByteString
