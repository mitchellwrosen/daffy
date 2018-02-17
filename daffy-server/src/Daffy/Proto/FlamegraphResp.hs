module Daffy.Proto.FlamegraphResp
  ( FlamegraphResp(..)
  ) where

import Daffy.Proto.Response (Response)

import Data.Aeson (ToJSON)

-- | Relative filepath to the flamegraph @.svg@.
newtype FlamegraphResp
  = FlamegraphResp Text
  deriving (ToJSON)

type instance Response FlamegraphResp = "flamegraph"
