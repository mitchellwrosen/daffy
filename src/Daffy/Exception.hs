module Daffy.Exception
  ( DaffyException(..)
  ) where

import Control.Exception
import Data.Aeson (ToJSON, (.=), object, toJSON)
import Data.Text (Text)
import Data.Typeable

data DaffyException
  = DaffyExitCodeException String [String] Int
  | DaffyStatsParseException Text String
  | DaffyInfoParseException String
  deriving (Show, Typeable)

instance Exception DaffyException

instance ToJSON DaffyException where
  toJSON = \case
    DaffyExitCodeException path args code ->
      object
        [ "error" .= ("process terminated with non-zero exit code" :: Text)
        , "payload" .= object
          [ "path" .= path
          , "args" .= args
          , "code" .= code
          ]
        ]

    DaffyStatsParseException stats message ->
      object
        [ "error" .= ("failed to parse '-S' output" :: Text)
        , "payload" .= object
          [ "stats" .= stats
          , "message" .= message
          ]
        ]

    DaffyInfoParseException info ->
      object
        [ "error" .= ("failed to parse '+RTS --info' output" :: Text)
        , "payload" .= object
          [ "info" .= info
          ]
        ]
