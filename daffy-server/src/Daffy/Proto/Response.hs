{-# language MagicHash #-}

module Daffy.Proto.Response
  ( Response
  , json
  ) where

import Data.Aeson (ToJSON, (.=))
import GHC.Prim (proxy#)

import qualified Data.Aeson as Aeson

-- | Map a response type to a unique (hence the type family dependency) string
-- tag that identifies it.
type family Response (a :: Type)
  = (r :: Symbol)
  | r -> a

-- | Encode a 'Response' as JSON.
json :: forall a. (KnownSymbol (Response a), ToJSON a) => a -> LByteString
json response =
  Aeson.encode
    (Aeson.object
      [ "type" .= symbolVal' @(Response a) proxy#
      , "payload" .= response
      ])
