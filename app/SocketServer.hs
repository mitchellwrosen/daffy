module Main where

import Daffy.Process (Output(..), ProcessSnapshot(..), snapshot, spawn)

import Control.Monad
import Control.Monad.Managed
import Control.Monad.Reader
import Control.Exception.Safe (catchAny)
import Data.ByteString (ByteString)
import Data.IORef
import Data.IntMap (IntMap)
import Data.Monoid
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import System.Directory (removeFile)
import System.Exit (ExitCode(..))
import Text.Read (readMaybe)

import qualified Control.Exception as Exception (bracket, finally)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.IntMap as IntMap
import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Network.Socket.ByteString as Socket
import qualified Streaming.Prelude as Streaming

data Env = Env
  { envSocket :: Socket
  , envProcessInfo :: IORef (IntMap ProcessInfo)
  , envNextId :: IORef Int
  }

data ProcessInfo = ProcessInfo
  { processInfoSnapshot :: IO ProcessSnapshot
  }

debug :: Bool
debug = True

main :: IO ()
main = runManaged $ do
  -- Register a cleanup action to remove the unix socket file)
  finally (catchAny (removeFile "daffy.sock") (\_ -> pure ()))

  infoRef :: IORef (IntMap ProcessInfo) <-
    liftIO (newIORef mempty)

  nextIdRef :: IORef Int <-
    liftIO (newIORef 0)

  sock :: Socket <-
    bracket (socket AF_UNIX Stream defaultProtocol) close

  liftIO (bind sock (SockAddrUnix "daffy.sock"))
  liftIO (listen sock 1)

  forever $ do
    (sock', _) <-
      bracket (accept sock) (close . fst)

    let env :: Env
        env = Env
          { envSocket = sock'
          , envProcessInfo = infoRef
          , envNextId = nextIdRef
          }

    liftIO (runReaderT server env)

server :: ReaderT Env IO ()
server = do
  Env sock infoRef nextIdRef <- ask

  let loop :: IO ()
      loop = do
        bytes :: ByteString
          <- recv sock 4096

        if
          | Just cmd <- parseRUN bytes -> do
              snap :: IO ProcessSnapshot <-
                snapshot (spawn cmd)

              pid :: Int <-
                atomicModifyIORef' nextIdRef (\n -> (n+1, n))

              let info :: ProcessInfo
                  info = ProcessInfo
                    { processInfoSnapshot = snap }

              atomicModifyIORef' infoRef (\m -> (IntMap.insert pid info m, ()))

              send sock ("OK " <> Char8.pack (show pid))

              loop

          | Just pid <- parseOUTPUT bytes -> do
              info :: IntMap ProcessInfo <-
                readIORef infoRef

              case IntMap.lookup pid info of
                Nothing -> pure ()
                Just ProcessInfo{..} -> do
                  processInfoSnapshot >>= \case
                    ProcessRunning output stream -> do
                      forM_ (reverse output) (sendOutput sock)
                      code <- Streaming.mapM_ (sendOutput sock) stream
                      sendCode sock code
                      loop
                    ProcessFinished output code -> do
                      forM_ (reverse output) (sendOutput sock)
                      sendCode sock code

          | otherwise -> pure ()

  liftIO loop
 where
  sendOutput :: Socket -> Output -> IO ()
  sendOutput sock = \case
    Stdout line -> send sock ("STDOUT " <> Text.encodeUtf8 line)
    Stderr line -> send sock ("STDERR " <> Text.encodeUtf8 line)

  sendCode :: Socket -> ExitCode -> IO ()
  sendCode sock = \case
    ExitSuccess -> send sock "CODE 0"
    ExitFailure code -> send sock ("CODE " <> Char8.pack (show code))

  parseRUN :: ByteString -> Maybe String
  parseRUN = fmap Char8.unpack . ByteString.stripPrefix "RUN "

  parseOUTPUT :: ByteString -> Maybe Int
  parseOUTPUT = ByteString.stripPrefix "OUTPUT " >=> readMaybe . Char8.unpack


recv :: MonadIO m => Socket -> Int -> m ByteString
recv sock n = liftIO $ do
  bytes <- Socket.recv sock n
  when debug $
    if ByteString.null bytes
      then putStrLn "[PEER CLOSE]"
      else Char8.putStrLn ("[RECV] " <> bytes)
  pure bytes

send :: MonadIO m => Socket -> ByteString -> m ()
send sock bytes = liftIO $ do
  when debug (Char8.putStrLn ("[SEND] " <> bytes))
  Socket.sendAll sock bytes

bracket :: IO a -> (a -> IO b) -> Managed a
bracket acquire release = managed (Exception.bracket acquire release)

finally :: IO () -> Managed ()
finally cleanup = managed_ (`Exception.finally` cleanup)
