module Daffy.Eventlog
  ( parse
  , stream
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

-- Incrementally parsing an eventlog is a bit of a struggle. GHC is writing
-- binary event data to a file, so we use the "ghc-events" library to
-- parse the bytes as they come. So far, so good... until we hit EOF! If GHC
-- hasn't closed the file, then more events are to come, at which point we will
-- no longer be at EOF.
--
-- So, we use inotify to register interest in "modify" and "close" events on the
-- file, and thus, the simplest, single-celled "pub-sub" abstraction in the
-- world was born: an MVar inside a TMVar.
--
-- An *empty* TMVar indicates that there is *no subscriber*, so any "modify"
-- events that the "publisher" (inotify callback thread) might publish should
-- just be thrown away (without an MVar, there isn't anywhere to publish *to*).
-- The lack of interested subscriber at this moment is totally normal: during
-- incremental parsing of the eventlog, yet more events were appended to the
-- file.
--
-- A *full* TMVar indicates that there *is* a subscriber: at some point in the
-- past, EOF was reached by the incremental parser, and it would like to be
-- woken up are more bytes to be read. How do we let it know? We put into the
-- MVar it left for us!
--
-- Then there is the teardown dance: the publisher, should it receive a "closed"
-- event to publish, should definitely not throw it away if there happens to be
-- no subscriber at the moment, because unlike "modify" events, only one "close"
-- event will ever be published, and it's imperative the subscriber sees it. So,
-- the publisher blocks, waiting for an MVar to appear for it to put into.
--
-- The subscriber, in turn, will eventually receive the "close" message. At this
-- point it should continue reading until EOF one last time (in case more bytes
-- were written in-between the subscriber seeing EOF and actually subscribing),
-- after which it will have seen all the bytes.

--------------------------------------------------------------------------------
-- Streaming eventlog parsing

-- | @stream logfile@ incrementally parses an eventlog file.
stream :: FilePath -> Stream (Of GHC.Event) Managed ()
stream path = do
  var :: PubSub <-
    io newEmptyTMVarIO

  inotify :: INotify <-
    lift (managed withINotify)

  void . io $ do
    let events :: [INotify.EventVariety]
        events =
          [INotify.Modify, INotify.Close]

    let publisher :: INotify.Event -> IO ()
        publisher = \case
          INotify.Modified{} ->
            publish var EventlogModified
          INotify.Closed{} ->
            publish var EventlogClosed
          _ ->
            pure ()

    INotify.addWatch inotify events path publisher

  handle :: Handle <-
    lift (managed (withBinaryFile path ReadMode))

  hoist io
    (eventStream path GHC.decodeEventLog
      (streamHandle (subscribe var) handle))

streamHandle :: IO EventlogEvent -> Handle -> SByteString IO ()
streamHandle waitEvent handle =
  loop False
 where
  loop :: Bool -> SByteString IO ()
  loop done = do
    bytes :: ByteString <-
      io (ByteString.hGetSome handle defaultChunkSize)
    if ByteString.null bytes
      then
        unless done $
          io waitEvent >>= \case
            EventlogModified ->
              loop False
            EventlogClosed ->
              loop True
      else do
        SByteString.chunk bytes
        loop False

type PubSub
  = TMVar (MVar EventlogEvent)

data EventlogEvent
  = EventlogModified
  | EventlogClosed

publish :: PubSub -> EventlogEvent -> IO ()
publish var = \case
  EventlogModified ->
    atomically (tryTakeTMVar var) >>= \case
      Nothing ->
        pure ()
      Just box ->
        putMVar box EventlogModified
  EventlogClosed -> do
    box :: MVar EventlogEvent <-
      atomically (takeTMVar var)
    putMVar box EventlogClosed

subscribe :: PubSub -> IO EventlogEvent
subscribe var = do
  box :: MVar EventlogEvent <-
    newEmptyMVar
  atomically (tryPutTMVar var box) >>= \case
    False ->
      error "eventlogEvent: TMVar full"
    True ->
      takeMVar box

--------------------------------------------------------------------------------
-- Batch eventlog parsing

-- | @parse logfile@ parses an eventlog file, assumed to be fully written.
parse :: FilePath -> Handle -> Stream (Of GHC.Event) IO ()
parse path =
  eventStream path GHC.decodeEventLog . SByteString.fromHandle

--------------------------------------------------------------------------------
-- Common functionality

-- We stitch together a "Decoder Event" (streaming consumer of bytes, producer
-- of events) and a "Streaming ByteString" (streaming producer of bytes), and
-- out comes a "Stream Of Event" (streaming producer of events).
eventStream
  :: FilePath
  -> GHC.Decoder GHC.Event
  -> SByteString IO ()
  -> Stream (Of GHC.Event) IO ()
eventStream path decoder bytes =
  case decoder of
    GHC.Consume k ->
      io (SByteString.unconsChunk bytes) >>= \case
        Nothing ->
          pure ()
        Just (chunk, bytes') ->
          eventStream path (k chunk) bytes'
    GHC.Produce event decoder' -> do
      yields (event :> ())
      eventStream path decoder' bytes
    GHC.Done _ ->
      error "Done"
    GHC.Error _ err ->
      lift (throwIO (EventlogParseException err path))
