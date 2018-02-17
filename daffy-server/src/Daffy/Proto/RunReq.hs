module Daffy.Proto.RunReq
  ( RunReq(..)
  ) where

import Data.Aeson (FromJSON, Value, (.:), parseJSON, withObject)
import Data.Aeson.Types (Parser)

import qualified Data.List.NonEmpty as List1

data RunReq = RunReq
  { command :: List1 Char -- ^ Raw shell command.
  , stats :: Bool -- ^ Generate runtime stats?
  , prof :: Bool -- ^ Generate a time and allocation profile?
  , eventlog :: Bool -- ^ Write an eventlog?
  } deriving (Generic)

instance FromJSON RunReq where
  parseJSON :: Value -> Parser RunReq
  parseJSON =
    withObject "RunReq" $ \o -> do
      command_ <- o .: "command"
      when (null command_)
        (fail "Empty command")
      let command = List1.fromList command_
      stats <- o .: "stats"
      prof <- o .: "prof"
      eventlog <- o .: "eventlog"
      pure RunReq{..}
