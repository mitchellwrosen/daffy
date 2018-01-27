{-# options_ghc -fno-warn-name-shadowing #-}

module Main where

import Daffy.Prelude

import Data.Aeson (FromJSON)

import qualified Data.Aeson as Aeson
import qualified Network.WebSockets as WebSockets

-- import Daffy.Exception (DaffyException)
-- import Daffy.Info (parseInfo)
-- 
-- import qualified Daffy (run)
-- 
-- import Control.Exception
-- import Data.Aeson (ToJSON, (.=), encode, object)
-- import Network.HTTP.Types.Status (status200, status400, status404, status500)
-- import Network.Wai
-- 
-- import qualified Data.Text as Text
-- import qualified Network.Wai.Handler.Warp as Warp

-- stack exec foo

data Command = Command
  { command :: Text
  } deriving (Generic)

instance FromJSON Command

main :: IO ()
main =
  WebSockets.runServer "localhost" 8080 app

app :: WebSockets.PendingConnection -> IO ()
app pconn = do
  conn :: WebSockets.Connection <-
    WebSockets.acceptRequest pconn
  
  cmd :: Command <- do
    blob :: LByteString <-
      WebSockets.receiveData conn

    case Aeson.decode blob of
      Nothing -> do
        hPutStrLn stderr "Could not decode blobby blob"
        exitFailure
      Just request ->
        pure request

  pure ()

-- app :: Application
-- app req resp = do
--   let json200, json400, json500 :: ToJSON a => a -> IO ResponseReceived
--       json200 = resp . responseLBS status200 [] . encode
--       json400 = resp . responseLBS status400 [] . encode
--       json500 = resp . responseLBS status500 [] . encode
-- 
--       exception :: SomeException -> IO ResponseReceived
--       exception ex = do
--         case fromException ex of
--           Just (ex :: DaffyException) -> json500 ex
--           _ -> json400 (object ["error" .= show ex])
-- 
--   case (requestMethod req, pathInfo req) of
--     ("GET", ["info", path]) ->
--       handle exception $ do
--         info <- parseInfo (Text.unpack path)
--         json200 (object ["info" .= info])
-- 
--     ("POST", "run":prog:args) ->
--       handle exception $ do
--         stats <- Daffy.run (Text.unpack prog) (map Text.unpack args)
--         json200 (object ["stats" .= stats])
-- 
--     _ -> resp (responseLBS status404 [] "")
