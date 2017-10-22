module Main where

import qualified Daffy

import Control.Monad
import Data.Aeson (Value, encode, toJSON)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Network.HTTP.Types.Status (status200, status404, status500)
import Network.Wai
import System.Environment
import System.Exit

import qualified Data.ByteString.Lazy.Char8 as LChar8
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
app prog args req resp =
  case (requestMethod req, pathInfo req) of
    ("GET", ["name"]) ->
      resp (responseLBS status200 [] (LChar8.pack (unwords (prog:args))))

    ("POST", ["run"]) ->
      Daffy.run prog args >>= \case
        Left ex -> do
          let json :: HashMap Text Text
              json = HashMap.singleton "error" (Text.pack (show ex))

          resp (responseLBS status500 [] (encode json))

        Right stats -> do
          let json :: HashMap Text Value
              json = HashMap.singleton "stats" (toJSON stats)

          resp (responseLBS status200 [] (encode json))

    _ -> resp (responseLBS status404 [] "")
