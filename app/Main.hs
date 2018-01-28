{-# options_ghc -fno-warn-name-shadowing #-}

module Main where

import Data.Aeson (FromJSON)

import qualified Data.Aeson as Aeson
import qualified Network.WebSockets as WebSockets

data Command = Command
  { command :: String
  } deriving (Generic)

instance FromJSON Command

main :: IO ()
main =
  WebSockets.runServer "localhost" 8080 app

app :: WebSockets.PendingConnection -> IO ()
app pconn = do
  conn :: WebSockets.Connection <-
    WebSockets.acceptRequest pconn

  Command{command} <- do
    blob :: LByteString <-
      WebSockets.receiveData conn

    case Aeson.decode blob of
      Nothing -> do
        hPutStrLn stderr "Could not decode blobby blob"
        exitFailure
      Just request ->
        pure request

  pure ()
