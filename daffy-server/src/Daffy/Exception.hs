module Daffy.Exception
  ( DaffyException(..)
  ) where

import Data.List (intercalate)

import qualified Data.ByteString.Lazy as LByteString

data DaffyException
  = CommandParseException LByteString [Char]
  | EventlogParseException [Char]
  | InfoParseException LByteString
  | StatsParseException Text [Char]
  deriving (Show, Typeable)

instance Exception DaffyException where
  displayException :: DaffyException -> [Char]
  displayException = \case
    CommandParseException bytes err ->
      intercalate "\n"
        [ "Failed to parse command: " ++ err
        , unpack (decodeUtf8 (LByteString.toStrict bytes))
        ]

    EventlogParseException err ->
      "Failed to parse event log: " ++ err

    InfoParseException bytes ->
      intercalate "\n"
        [ "Failed to parse runtime info:"
        , unpack (decodeUtf8 (LByteString.toStrict bytes))
        ]

    StatsParseException bytes err ->
      intercalate "\n"
        [ "Failed to parse runtime stats: " ++ err
        , unpack bytes
        ]
