module Daffy.Proto.ErrorResp
  ( ErrorResp(..)
  ) where

import Daffy.Proto.Response (Response)

import Data.Aeson (ToJSON)

data ErrorResp = ErrorResp
  { message :: !Text }
  deriving (Generic)

instance ToJSON ErrorResp

type instance Response ErrorResp = "error"
