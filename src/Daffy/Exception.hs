module Daffy.Exception
  ( DaffyException(..)
  ) where

import Control.Exception
import Data.Typeable

data DaffyException
  = DaffyStatsParseException String
  | DaffyInfoParseException String
  deriving (Show, Typeable)

instance Exception DaffyException
