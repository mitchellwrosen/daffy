{-# options_ghc -fno-warn-partial-type-signatures #-}

module Daffy.Eventlog
  ( parse
  , stream
    -- ** Eventlog analysis
  , World(..)
  , Cap(..)
  , Capset(..)
  , initialWorld
  , stepWorld
  ) where

import Daffy.Exception

import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Streaming
import System.INotify (INotify, withINotify)
import System.IO (IOMode(ReadMode), withBinaryFile)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Streaming as SByteString
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified GHC.RTS.Events as GHC
import qualified GHC.RTS.Events.Incremental as GHC
import qualified System.INotify as INotify

-- Incrementally parsing an eventlog is a bit of a struggle. GHC is writing
-- binary event data to a file, so we use the "ghc-events" library to
-- parse the bytes as they come. So far, so good... until we hit EOF! If GHC
-- hasn't closed the file, then more events are to come, at which point we will
-- no longer be at EOF.
--
-- 'threadWaitRead' (i.e. 'select' or 'epoll' syscall) is not an option here,
-- because the eventlog is just a regular file, not a FIFO or a socket.
--
-- So, we use inotify to register interest in "modify" and "close" events on the
-- file, and thus, the simplest, single-celled "pub-sub" abstraction in the
-- world was born: an MVar inside a TMVar.
--
-- We have one "publisher": the inofity callback thread that occasionally sees
-- and forwards "modify" or "close" events.
--
-- We have one "subscriber": the thread that is incrementally parsing the
-- eventlog, which, upon reaching EOF, becomes interested in the inotify events
-- being published.
--
-- An *empty* TMVar indicates that there is *no subscriber*, so any "modify"
-- events that the publisher might publish should just be thrown away (without
-- an MVar, there isn't anywhere to publish *to*). The lack of interested
-- subscriber at this moment is totally normal: during incremental parsing of
-- the eventlog, yet more events were appended to the end of the file before we
-- got there.
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
    (eventStream GHC.decodeEventLog
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
parse :: Handle -> Stream (Of GHC.Event) IO ()
parse =
  eventStream GHC.decodeEventLog . SByteString.fromHandle

--------------------------------------------------------------------------------
-- Common functionality

-- We stitch together a "Decoder Event" (streaming consumer of bytes, producer
-- of events) and a "Streaming ByteString" (streaming producer of bytes), and
-- out comes a "Stream Of Event" (streaming producer of events).
eventStream
  :: GHC.Decoder GHC.Event
  -> SByteString IO ()
  -> Stream (Of GHC.Event) IO ()
eventStream decoder bytes =
  case decoder of
    GHC.Consume k ->
      io (SByteString.unconsChunk bytes) >>= \case
        Nothing ->
          pure ()
        Just (chunk, bytes') ->
          eventStream (k chunk) bytes'
    GHC.Produce event decoder' -> do
      yields (event :> ())
      eventStream decoder' bytes
    GHC.Done _ ->
      error "Done"
    GHC.Error _ err ->
      lift (throw (EventlogParseException err))

--------------------------------------------------------------------------------
-- Eventlog analysis

data World = World
  { worldTimestamp :: !Word64
  , worldCaps :: !(IntMap Cap)
  , worldCapsets :: !(IntMap Capset)
  }

-- | Capability.
data Cap = Cap
  { capId :: !Int
    -- ^ Capability id.
  , capCapsets :: !IntSet
    -- ^ The capability set ids this capability belongs to.
  }

-- | A set of capabilities.
data Capset = Capset
  { capsetId :: !Word32
    -- ^ Capability set id.
  , capsetType :: !GHC.CapsetType
    -- ^ The type of capability set.
  , capsetCaps :: !IntSet
    -- ^ The set of capability ids that make up this capability set.
  }

initialWorld :: World
initialWorld =
  World
    { worldTimestamp = 0
    , worldCaps = mempty
    , worldCapsets = mempty
    }

-- | Step the 'World' forward with an event from the eventlog.
stepWorld :: GHC.Event -> World -> World
stepWorld event (setTimestamp (GHC.evTime event) -> world) =
  case GHC.evSpec event of
    -- Insert into the capset's caps and the cap's capsets.
    GHC.CapsetAssignCap id cap_id ->
      world
        { worldCaps =
            IntMap.adjust
              (\cap ->
                cap
                  { capCapsets =
                      IntSet.insert (fromIntegral id) (capCapsets cap)
                  })
              cap_id
              (worldCaps world)
        , worldCapsets =
            IntMap.adjust
              (\capset ->
                capset
                  { capsetCaps =
                      IntSet.insert cap_id (capsetCaps capset)
                  })
              (fromIntegral id)
              (worldCapsets world)
        }

    -- Create a new empty capset in 'worldCapsets'
    GHC.CapsetCreate id type_ ->
      world
        { worldCapsets =
            IntMap.insert
              (fromIntegral id)
              (Capset
                { capsetId = id
                , capsetType = type_
                , capsetCaps = mempty
                })
              (worldCapsets world)
        }

    -- Delete the capset from 'worldCapsets', and for each cap that was in it,
    -- delete the capset from its 'capCapsets'.
    GHC.CapsetDelete id ->
      let
        capsets :: IntMap Capset
        capsets =
          worldCapsets world

        new_caps :: IntMap Cap
        new_caps =
          IntSet.foldl'
            (\caps cap_id ->
              IntMap.adjust
                (\cap ->
                  cap
                    { capCapsets =
                        IntSet.delete (fromIntegral id) (capCapsets cap)
                    })
                cap_id
                caps)
            (worldCaps world)
            (capsetCaps (capsets IntMap.! fromIntegral id))
      in
        world
          { worldCaps = new_caps
          , worldCapsets = IntMap.delete (fromIntegral id) capsets
          }
    _ ->
      world

setTimestamp :: Word64 -> World -> World
setTimestamp timestamp world =
  world { worldTimestamp = timestamp }
