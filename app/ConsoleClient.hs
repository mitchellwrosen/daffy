module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.Managed
import Data.ByteString (ByteString)
import Data.Monoid
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Text.Read (readMaybe)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Control.Exception as Exception (bracket)
import qualified Network.Socket.ByteString as Socket

debug :: Bool
debug = True

main :: IO ()
main = runManaged $ do
  sock :: Socket <-
    bracket (socket AF_UNIX Stream defaultProtocol) close

  liftIO (connect sock (SockAddrUnix "daffy.sock"))

  send sock "RUN ls -al"

  Just pid <-
    parseRUN_OK <$> recv sock 4096

  send sock ("OUTPUT " <> Char8.pack (show pid))

  let loop :: IO ()
      loop = do
        bytes <- recv sock 4096
        unless ("CODE" `ByteString.isPrefixOf` bytes) loop

  liftIO loop

 where
  parseRUN_OK :: ByteString -> Maybe Int
  parseRUN_OK = ByteString.stripPrefix "OK " >=> readMaybe . Char8.unpack

recv :: MonadIO m => Socket -> Int -> m ByteString
recv sock n = liftIO $ do
  bytes <- Socket.recv sock n
  when debug (Char8.putStrLn ("[RECV] " <> bytes))
  pure bytes

send :: MonadIO m => Socket -> ByteString -> m ()
send sock bytes = liftIO $ do
  when debug (Char8.putStrLn ("[SEND] " <> bytes))
  Socket.sendAll sock bytes

bracket :: IO a -> (a -> IO b) -> Managed a
bracket acquire release = managed (Exception.bracket acquire release)
