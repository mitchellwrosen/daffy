module Daffy.Eventlog
  ( parse
  ) where

import Daffy.Exception

import Streaming
import System.IO (IOMode(ReadMode))

import qualified Data.ByteString.Streaming as SByteString
import qualified Data.ByteString.Streaming.Internal as SByteString
import qualified GHC.RTS.Events as GHC (Event, Header)
import qualified GHC.RTS.Events.Incremental as GHC

-- | @parse logfile@ incrementally parses an eventlog file.
parse :: String -> Managed (Stream (Of GHC.Event) IO ())
parse log = do
  handle :: Handle <-
    managed (withFile log ReadMode)
  (header, body) :: (GHC.Header, SByteString IO ()) <-
    liftIO (parseHeader (SByteString.fromHandle handle))
  pure (parseEvents (GHC.decodeEvents header) body)

parseHeader :: SByteString IO () -> IO (GHC.Header, SByteString IO ())
parseHeader =
  go GHC.decodeHeader
 where
  go :: GHC.Decoder GHC.Header
     -> SByteString IO ()
     -> IO (GHC.Header, SByteString IO ())
  go decoder bytes =
    case decoder of
      GHC.Consume k ->
        SByteString.unconsChunk bytes >>= \case
          Nothing ->
            throw EventlogParseException
          Just (chunk, bytes') ->
            go (k chunk) bytes'
      GHC.Produce header (GHC.Done leftover) ->
        pure (header, SByteString.consChunk leftover bytes)
      GHC.Produce _ _ ->
        error "Produce"
      GHC.Done _ ->
        error "Done"
      GHC.Error _ _ ->
        throw EventlogParseException

parseEvents
  :: GHC.Decoder GHC.Event -> SByteString IO () -> Stream (Of GHC.Event) IO ()
parseEvents decoder bytes =
  case decoder of
    GHC.Consume k ->
      liftIO (SByteString.unconsChunk bytes) >>= \case
        Nothing ->
          pure ()
        Just (chunk, bytes') ->
          parseEvents (k chunk) bytes'
    GHC.Produce event decoder' -> do
      yields (event :> ())
      parseEvents decoder' bytes
    GHC.Done _ ->
      error "Done"
    GHC.Error _ _ ->
      lift (throw EventlogParseException)
