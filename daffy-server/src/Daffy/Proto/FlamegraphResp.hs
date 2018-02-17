module Daffy.Proto.FlamegraphResp
  ( FlamegraphResp(..)
  ) where

import Data.Aeson (ToJSON)

data FlamegraphResp = FlamegraphResp
  { type_ :: !Text -- ^ @"flamegraph"@
  , path :: !Text -- ^ Relative filepath to the .svg
  } deriving (Generic)

instance ToJSON FlamegraphResp
