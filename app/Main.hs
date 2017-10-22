module Main where

import Daffy.Exception (DaffyException)
import Daffy.Info (parseInfo)

import qualified Daffy (run)

import Control.Exception
import Control.Monad
import Data.Aeson (ToJSON, Value, encode, toJSON)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Network.HTTP.Types.Status (status200, status400, status404, status500)
import Network.Wai
import System.Environment
import System.Exit

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  args :: [String] <-
    getArgs

  when (length args == 0)
    exitFailure

  Warp.run 8080 (app (head args) (tail args))

app :: String -> [String] -> Application
app prog args req resp = do
  let json200, json400, json500 :: ToJSON a => a -> IO ResponseReceived
      json200 = resp . responseLBS status200 [] . encode
      json400 = resp . responseLBS status400 [] . encode
      json500 = resp . responseLBS status500 [] . encode

      exception :: SomeException -> IO ResponseReceived
      exception ex = do
        let response :: HashMap Text Value
            response = HashMap.singleton "error" (toJSON (show ex))

        case fromException ex of
          Just (_ :: DaffyException) -> json500 response
          _ -> json400 response

  case (requestMethod req, pathInfo req) of
    ("GET", ["name"]) -> do
      let response :: HashMap Text Value
          response = HashMap.singleton "name" (toJSON (unwords (prog:args)))

      json200 response

    ("GET", ["info", path]) ->
      handle exception $ do
        info <- parseInfo (Text.unpack path)

        let response :: HashMap Text Value
            response = HashMap.singleton "info" (toJSON info)

        json200 response

    ("POST", ["run"]) ->
      handle exception $ do
        stats <- Daffy.run prog args

        let json :: HashMap Text Value
            json = HashMap.singleton "stats" (toJSON stats)

        resp (responseLBS status200 [] (encode json))

    _ -> resp (responseLBS status404 [] "")
