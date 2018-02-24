module Daffy.Stats.Internal where

import GHC.Generics (Generic)

data Stats = Stats
  { garbageCollections :: ![GCStats]
  , generationSummaries :: ![GenStats]
  , parallelGarbageCollection :: !(Maybe ParallelGCStats)
  , tasks :: !(Maybe TasksStats)
  , sparks :: !(Maybe SparksStats)
  , runtimeInitTime :: !Time
  , mutatorTime :: !Time
  , garbageCollectionTime :: !Time
  , retainerProfilingTime :: !(Maybe Time)
  , otherProfilingTime :: !(Maybe Time)
  , runtimeShutdownTime :: !Time
  , totalTime :: !Time
  , percentGarbageCollectionTime :: !(Maybe Time)
  } deriving (Eq, Generic, Show)

data GCStats = GCStats
  { bytesAllocated :: !Int
  , bytesCopied :: !Int
  , liveBytes :: !Int
  , time :: !Time
  , totalTime :: !Time
  , pageFaults :: !Int
  , totalPageFaults :: !Int
  , generation :: !Int
  } deriving (Eq, Generic, Show)

data GenStats = GenStats
  { parallel :: !Int
  , averagePauseTime :: !Double
  , maxPauseTime :: !Double
  } deriving (Eq, Generic, Show)

data ParallelGCStats = ParallelGCStats
  { parallel :: !Double
  , serial :: !Double
  , perfect :: !Double
  } deriving (Eq, Generic, Show)

data TasksStats = TasksStats
  { tasks :: !Int
  , bound :: !Int
  , peakWorkers :: !Int
  , totalWorkers :: !Int
  } deriving (Eq, Generic, Show)

data SparksStats = SparksStats
  { sparks :: !Int
  , converted :: !Int
  , overflowed :: !Int
  , dud :: !Int
  , garbageCollected :: !Int
  , fizzled :: !Int
  } deriving (Eq, Generic, Show)

data Time = Time
  { user :: !Double
  , elapsed :: !Double
  } deriving (Eq, Generic, Show)
