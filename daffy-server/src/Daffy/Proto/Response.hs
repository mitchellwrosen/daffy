module Daffy.Proto.Response
  ( Response
  ) where

type family Response (a :: Type)
  = (r :: Symbol)
  | r -> a
