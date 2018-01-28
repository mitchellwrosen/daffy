module Prelude
  ( module X
  , LByteString
  , SByteString
  ) where

import Control.Applicative as X
import Control.Concurrent.STM as X
import Control.Monad as X
import Control.Monad.IO.Class as X
import Control.Monad.Managed as X
import Control.Exception as X (Exception)
import Control.Exception.Safe as X (throw)
import Data.ByteString as X (ByteString)
import Data.Either as X
import Data.Function as X ((&))
import Data.Text as X (Text, unpack)
import Data.Text.Encoding as X (decodeUtf8, encodeUtf8)
import Data.Typeable as X (Typeable)
import Debug.Trace as X
import GHC.Generics as X (Generic)
import "base" Prelude as X hiding (log)
import System.IO as X (Handle, hPutStrLn, stderr, withFile)
import System.Exit as X (ExitCode(..), exitFailure)
import Text.Read as X (readMaybe)

import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Streaming

type LByteString
  = Data.ByteString.Lazy.ByteString

type SByteString
  = Data.ByteString.Streaming.ByteString
