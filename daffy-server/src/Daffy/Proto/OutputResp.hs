module Daffy.Proto.OutputResp
  ( OutputResp(..)
  ) where

import Daffy.Proto.Response

import Data.Aeson (ToJSON)

data OutputResp = OutputResp
  { line :: !Text -- ^ A line of output.
  , stdout :: !Bool -- ^ True if stdout, False if stderr.
  } deriving (Generic)

instance ToJSON OutputResp

type instance Response OutputResp = "output"
