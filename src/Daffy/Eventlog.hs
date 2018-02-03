module Daffy.Eventlog
  ( parse
  ) where

import Daffy.Exception

import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Streaming
import System.INotify (INotify, withINotify)
import System.IO (IOMode(ReadMode), withBinaryFile)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Streaming as SByteString
import qualified GHC.RTS.Events as GHC (Event)
import qualified GHC.RTS.Events.Incremental as GHC
import qualified System.INotify as INotify

-- The simplest, single-celled "pub-sub" abstraction in the world: an MVar
-- inside a TMVar.
--
-- If there is a subscriber (i.e. someone interested in an EventlogEvent), they
-- will fill the TMvar with an empty MVar. When the publisher has an event, they
-- will fill the MVar (if any), and clear the TVar in the process. Thus, events
-- produced when there is no interested subscriber are dropped on the floor.
--
-- As a special ending condition, we don't care if there is no subscriber when
-- we have an 'EventLogClosed' message to publish (the final event), because
-- we know the subscriber will eventually want to see this message, even
-- though they happen to be busy right now. So, we block, waiting for an MVar
-- to appear.
type Cell
  = TMVar (MVar EventlogEvent)

data EventlogEvent
  = EventlogModified
  | EventlogClosed

eventlogModified :: Cell -> IO ()
eventlogModified var =
  atomically (tryTakeTMVar var) >>= \case
    Nothing ->
      pure ()
    Just box ->
      putMVar box EventlogModified

eventlogClosed :: Cell -> IO ()
eventlogClosed var = do
  box :: MVar EventlogEvent <-
    atomically (takeTMVar var)
  putMVar box EventlogClosed

eventlogEvent :: Cell -> IO EventlogEvent
eventlogEvent var = do
  box :: MVar EventlogEvent <-
    newEmptyMVar
  atomically (tryPutTMVar var box) >>= \case
    False ->
      error "eventlogEvent: TMVar full"
    True ->
      takeMVar box

-- | @parse logfile@ incrementally parses an eventlog file.
parse :: FilePath -> Stream (Of GHC.Event) Managed ()
parse path = do
  var :: Cell <-
    io newEmptyTMVarIO

  inotify :: INotify <-
    lift (managed withINotify)

  let f :: INotify.Event -> IO ()
      f = \case
        INotify.Modified{} ->
          eventlogModified var
        INotify.Closed{} -> do
          eventlogClosed var
        _ ->
          pure ()

  _ <- io (INotify.addWatch inotify [INotify.Modify, INotify.Close] path f)

  handle :: Handle <-
    lift (managed (withBinaryFile path ReadMode))

  hoist io
    (parseEvents GHC.decodeEventLog (fromHandle (eventlogEvent var) handle))

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

fromHandle :: IO EventlogEvent -> Handle -> SByteString IO ()
fromHandle waitEvent handle =
  loop
 where
  loop :: SByteString IO ()
  loop = do
    bytes :: ByteString <-
      io (ByteString.hGetSome handle defaultChunkSize)
    if ByteString.null bytes
      then do
        io waitEvent >>= \case
          EventlogModified ->
            loop
          EventlogClosed ->
            pure ()
      else do
        SByteString.chunk bytes
        loop
