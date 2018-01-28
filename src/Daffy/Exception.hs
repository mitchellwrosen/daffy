module Daffy.Exception
  ( DaffyException(..)
  ) where

import Data.Aeson (ToJSON, (.=), object, toJSON)

data DaffyException
  = DaffyStatsParseException Text String
  | DaffyInfoParseException
  deriving (Show, Typeable)

instance Exception DaffyException

instance ToJSON DaffyException where
  toJSON = \case
    DaffyStatsParseException stats message ->
      object
        [ "error" .= ("failed to parse '-S' output" :: Text)
        , "payload" .= object
          [ "stats" .= stats
          , "message" .= message
          ]
        ]

    DaffyInfoParseException ->
      object
        [ "type" .= ("error" :: Text)
        , "message" .= ("failed to parse '+RTS --info' output" :: Text)
        ]
