module Prelude
  ( module X
  , LByteString
  , List1
  , SByteString
  , forkIO_
  , hPutStrLn
  , identity
  , io
  , putStrLn
  , throw
  ) where

import Control.Applicative as X
import Control.Concurrent.Async as X
import Control.Concurrent.MVar as X
import Control.Concurrent.STM as X
import Control.Monad as X
import Control.Monad.IO.Class as X (MonadIO)
import Control.Monad.Managed as X
import Control.Exception as X (Exception, displayException)
import Data.ByteString as X (ByteString)
import Data.Either as X
import Data.Foldable as X (toList)
import Data.Function as X ((&), fix)
import Data.IntMap.Strict as X (IntMap)
import Data.IntSet as X (IntSet)
import Data.IORef as X
import Data.Kind as X (Type)
import Data.Semigroup as X ((<>))
import Data.Text as X (Text, pack, unpack)
import Data.Text.Encoding as X (decodeUtf8, encodeUtf8)
import Data.Typeable as X (Typeable)
import Data.Vector as X (Vector)
import Data.Word as X
import Debug.Trace as X
import GHC.Generics as X (Generic)
import GHC.TypeLits as X (KnownSymbol, Symbol, symbolVal')
import "base" Prelude as X hiding (String, id, log, putStrLn)
import System.IO as X (Handle, stderr)
import System.Exit as X (ExitCode(..), exitFailure)
import Text.Read as X (readMaybe)
import UnliftIO as X
  (MonadUnliftIO, catch, catchAny, finally, try, tryAny, withFile)
import UnliftIO.Concurrent as X (forkIO, threadDelay)

import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Streaming
import qualified Data.List.NonEmpty
import qualified "base" Prelude
import qualified System.IO
import qualified UnliftIO

type LByteString
  = Data.ByteString.Lazy.ByteString

type List1
  = Data.List.NonEmpty.NonEmpty

type SByteString
  = Data.ByteString.Streaming.ByteString

forkIO_ :: MonadUnliftIO m => m () -> m ()
forkIO_ =
  void . forkIO

hPutStrLn :: MonadIO m => Handle -> [Char] -> m ()
hPutStrLn handle =
  liftIO . withMVar iolock . const . System.IO.hPutStrLn handle

identity :: a -> a
identity =
  Prelude.id

io :: MonadIO m => IO a -> m a
io =
  liftIO

iolock :: MVar ()
iolock =
  unsafePerformIO (newMVar ())
{-# NOINLINE iolock #-}

putStrLn :: MonadIO m => [Char] -> m ()
putStrLn =
  liftIO . withMVar iolock . const . System.IO.putStrLn

throw :: (Exception e, MonadIO m) => e -> m a
throw =
  UnliftIO.throwIO
