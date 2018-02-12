module Daffy.Exception
  ( DaffyException(..)
  ) where

import Data.List (intercalate)

import qualified Data.ByteString.Lazy as LByteString

data DaffyException
  = CommandParseException LByteString [Char]
  | EventlogParseException [Char] FilePath
  | InfoParseException LByteString
  | ProfileParseException [Char] FilePath
  | StatsParseException [Char] FilePath
  deriving (Show, Typeable)

instance Exception DaffyException where
  displayException :: DaffyException -> [Char]
  displayException = \case
    CommandParseException bytes err ->
      intercalate "\n"
        [ "Failed to parse command: " ++ err
        , unpack (decodeUtf8 (LByteString.toStrict bytes))
        ]

    EventlogParseException err path ->
      "Failed to parse event log (" ++ path ++ "): " ++ err

    InfoParseException bytes ->
      intercalate "\n"
        [ "Failed to parse runtime info:"
        , unpack (decodeUtf8 (LByteString.toStrict bytes))
        ]

    ProfileParseException err path ->
      "Failed to parse profile (" ++ path ++ "): " ++ err

    StatsParseException err path ->
      "Failed to parse stats (" ++ path ++ "): " ++ err
