module Daffy.Proto.StatsResp
  ( StatsResp(..)
  ) where

import Daffy.Stats (Stats)

import Data.Aeson (ToJSON)

data StatsResp = StatsResp
  { type_ :: !Text -- ^ @"stats"@.
  , stats :: !Stats
  } deriving (Generic)

instance ToJSON StatsResp
