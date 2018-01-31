module Daffy.Command where

import Data.Aeson (FromJSON)

data Command = Command
  { command :: String
  , stats :: Bool
  } deriving (Generic)

instance FromJSON Command
