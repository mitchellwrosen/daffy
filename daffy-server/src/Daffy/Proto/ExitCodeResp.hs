module Daffy.Proto.ExitCodeResp
  ( ExitCodeResp(..)
  ) where

import Daffy.Proto.Response

import Data.Aeson (ToJSON)

newtype ExitCodeResp
  = ExitCodeResp Int
  deriving ToJSON

type instance Response ExitCodeResp = "exitcode"
