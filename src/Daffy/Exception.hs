module Daffy.Exception
  ( DaffyException(..)
  ) where

data DaffyException
  = EventlogParseException
  | InfoParseException
  | StatsParseException Text String
  deriving (Show, Typeable)

instance Exception DaffyException
