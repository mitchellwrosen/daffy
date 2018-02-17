module Daffy.Proto.ExitCodeResp
  ( ExitCodeResp(..)
  ) where

import Data.Aeson (ToJSON)

data ExitCodeResp = ExitCodeResp
  { type_ :: !Text -- ^ @"exitcode"@.
  , code :: !Int
  } deriving (Generic)

instance ToJSON ExitCodeResp
