module Daffy.Proto.OutputResp
  ( OutputResp(..)
  ) where

import Data.Aeson (ToJSON)

data OutputResp = OutputResp
  { type_ :: !Text -- @"output"@.
  , line :: !Text -- ^ A line of output.
  , stdout :: !Bool -- ^ True if stdout, False if stderr.
  } deriving (Generic)

instance ToJSON OutputResp
