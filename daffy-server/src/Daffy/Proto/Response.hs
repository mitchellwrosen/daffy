module Daffy.Proto.Response
  ( Response
  ) where

-- | Map a response type to a unique (hence the type family dependency) string
-- tag that identifies it.
type family Response (a :: Type)
  = (r :: Symbol)
  | r -> a
