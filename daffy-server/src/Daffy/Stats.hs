{-# options_ghc -fno-warn-name-shadowing #-}

-- | Parsing @+RTS -S -RTS@ stats.

module Daffy.Stats
  ( Stats(..)
  , GCStats(..)
  , GenStats(..)
  , ParallelGCStats(..)
  , TasksStats(..)
  , SparksStats(..)
  , Time(..)
  , parse
  ) where

import Data.Aeson (ToJSON)
import Data.Attoparsec.ByteString.Char8 hiding (parse)

import qualified Data.Text.Encoding as Text (encodeUtf8)

data Stats = Stats
  { garbageCollections           :: ![GCStats]
  , generationSummaries          :: ![GenStats]
  , parallelGarbageCollection    :: !(Maybe ParallelGCStats)
  , tasks                        :: !(Maybe TasksStats)
  , sparks                       :: !(Maybe SparksStats)
  , runtimeInitTime              :: !Time
  , mutatorTime                  :: !Time
  , garbageCollectionTime        :: !Time
  , retainerProfilingTime        :: !(Maybe Time)
  , otherProfilingTime           :: !(Maybe Time)
  , runtimeShutdownTime          :: !Time
  , totalTime                    :: !Time
  , percentGarbageCollectionTime :: !(Maybe Time)
  } deriving (Eq, Generic, Show)

instance ToJSON Stats

data GCStats = GCStats
  { bytesAllocated  :: !Int
  , bytesCopied     :: !Int
  , liveBytes       :: !Int
  , time            :: !Time
  , totalTime       :: !Time
  , pageFaults      :: !Int
  , totalPageFaults :: !Int
  , generation      :: !Int
  } deriving (Eq, Generic, Show)

instance ToJSON GCStats

data GenStats = GenStats
  { parallel         :: !Int
  , averagePauseTime :: !Double
  , maxPauseTime     :: !Double
  } deriving (Eq, Generic, Show)

instance ToJSON GenStats

data ParallelGCStats = ParallelGCStats
  { parallel :: !Double
  , serial   :: !Double
  , perfect  :: !Double
  } deriving (Eq, Generic, Show)

instance ToJSON ParallelGCStats

data TasksStats = TasksStats
  { tasks        :: !Int
  , bound        :: !Int
  , peakWorkers  :: !Int
  , totalWorkers :: !Int
  } deriving (Eq, Generic, Show)

instance ToJSON TasksStats

data SparksStats = SparksStats
  { sparks           :: !Int
  , converted        :: !Int
  , overflowed       :: !Int
  , dud              :: !Int
  , garbageCollected :: !Int
  , fizzled          :: !Int
  } deriving (Eq, Generic, Show)

instance ToJSON SparksStats

data Time = Time
  { user    :: !Double
  , elapsed :: !Double
  } deriving (Eq, Generic, Show)

instance ToJSON Time

parse :: Text -> Either [Char] Stats
parse =
  parseOnly statsParser . Text.encodeUtf8

statsParser :: Parser Stats
statsParser = do
  skipSpace
  string "Alloc" *> skipSpace
  string "Copied" *> skipSpace
  string "Live" *> skipSpace
  string "GC" *> skipSpace
  string "GC" *> skipSpace
  string "TOT" *> skipSpace
  string "TOT" *> skipSpace
  string "Page" *> skipSpace
  string "Flts" *> skipSpace
  string "bytes" *> skipSpace
  string "bytes" *> skipSpace
  string "bytes" *> skipSpace
  string "user" *> skipSpace
  string "elap" *> skipSpace
  string "user" *> skipSpace
  string "elap" *> skipSpace

  garbageCollections <- some gcStatsParser

  int *> skipSpace
  double *> skipSpace
  double *> skipSpace
  commaSepInt *> skipSpace
  string "bytes allocated in the heap" *> skipSpace
  commaSepInt *> skipSpace
  string "bytes copied during GC" *> skipSpace
  commaSepInt *> skipSpace
  _ <- string "bytes maximum residency ("
  _ <- int
  string " sample(s))" *> skipSpace
  commaSepInt *> skipSpace
  string "bytes maximum slop" *> skipSpace
  _ <- int
  _ <- string " MB total memory in use ("
  _ <- int
  string " MB lost due to fragmentation)" *> skipSpace
  string "Tot" *> skipSpace
  string "time" *> skipSpace
  string "(elapsed)" *> skipSpace
  string "Avg" *> skipSpace
  string "pause" *> skipSpace
  string "Max" *> skipSpace
  string "pause" *> skipSpace

  generationSummaries <- many genStatsParser

  parallelGarbageCollection <- optional parallelGCStatsParser
  tasks <- optional tasksStatsParser
  sparks <- optional sparksStatsParser

  let parseTimeBlock :: ByteString -> Parser Time
      parseTimeBlock name = do
        string name *> skipSpace
        string "time" *> skipSpace
        user <- double <* char 's' <* skipSpace
        char '(' *> skipSpace
        elapsed <- double <* char 's' <* skipSpace
        string "elapsed)" *> skipSpace
        pure Time{..}

  runtimeInitTime <- parseTimeBlock "INIT"
  mutatorTime <- parseTimeBlock "MUT"
  garbageCollectionTime <- parseTimeBlock "GC"
  retainerProfilingTime <- optional (parseTimeBlock "RP")
  otherProfilingTime <- optional (parseTimeBlock "PROF")
  runtimeShutdownTime <- parseTimeBlock "EXIT"
  totalTime <- parseTimeBlock "Total"

  percentGarbageCollectionTime <-
    optional $ do
      string "%GC" *> skipSpace
      string "time" *> skipSpace
      user <- percentage <* skipSpace
      elapsed <- char '(' *> double <* string "% elapsed)" <* skipSpace
      pure Time{..}

  string "Alloc rate" *> skipSpace
  commaSepInt *> skipSpace
  string "bytes per MUT second" *> skipSpace

  string "Productivity" *> skipSpace
  percentage *> skipSpace
  string "of total user," *> skipSpace
  percentage *> skipSpace
  string "of total elapsed" *> skipSpace

  _ <- optional (string "gc_alloc_block_sync: " *> int *> skipSpace)
  _ <- optional (string "whitehole_spin: " *> int *> skipSpace)
  _ <- many (string "gen[" *> int *> string "].sync: " *> int *> skipSpace)

  pure Stats{..}

gcStatsParser :: Parser GCStats
gcStatsParser = do
  bytesAllocated <- int <* skipSpace
  bytesCopied <- int <* skipSpace
  liveBytes <- int <* skipSpace

  user <- double <* skipSpace
  elapsed <- double <* skipSpace
  let time = Time{..}

  user <- double <* skipSpace
  elapsed <- double <* skipSpace
  let totalTime = Time{..}

  pageFaults <- int <* skipSpace
  totalPageFaults <- int <* skipSpace
  string "(Gen:" *> skipSpace
  generation <- int <* skipSpace
  char ')' *> skipSpace

  pure GCStats{..}

genStatsParser :: Parser GenStats
genStatsParser = do
  string "Gen" *> skipSpace
  int *> skipSpace
  int *> skipSpace
  string "colls," *> skipSpace
  parallel <- int <* skipSpace
  string "par" *> skipSpace
  double *> char 's' *> skipSpace
  double *> char 's' *> skipSpace
  averagePauseTime <- double <* char 's' <* skipSpace
  maxPauseTime <- double <* char 's' <* skipSpace
  pure GenStats{..}

parallelGCStatsParser :: Parser ParallelGCStats
parallelGCStatsParser = do
  string "Parallel GC work balance:" *> skipSpace
  parallel <- percentage <* skipSpace <* string "(serial "
  serial <- percentage <* string ", perfect "
  perfect <- percentage <* char ')' <* skipSpace
  pure ParallelGCStats{..}

tasksStatsParser :: Parser TasksStats
tasksStatsParser = do
  string "TASKS:" *> skipSpace
  tasks <- int <* skipSpace
  bound <- char '(' *> int <* string " bound," <* skipSpace
  peakWorkers <- int <* string " peak workers ("
  totalWorkers <- int <* string " total), using -N"
  optional int *> char ')' *> skipSpace
  pure TasksStats{..}

sparksStatsParser :: Parser SparksStats
sparksStatsParser = do
  string "SPARKS:" *> skipSpace
  sparks <- int <* skipSpace <* char '('
  converted <- int <* skipSpace <* string "converted," <* skipSpace
  overflowed <- int <* skipSpace <* string "overflowed," <* skipSpace
  dud <- int <* skipSpace <* string "dud," <* skipSpace
  garbageCollected <- int <* skipSpace <* string "GC'd," <* skipSpace
  fizzled <- int <* skipSpace <* string "fizzled)" <* skipSpace
  pure SparksStats{..}

int :: Parser Int
int = decimal

commaSepInt :: Parser [Int]
commaSepInt = sepBy1 decimal (char ',')

percentage :: Parser Double
percentage =
  dbl <* char '%'
 where
  dbl :: Parser Double
  dbl = double
    <|> optional (char '-') *> string "nan" *> pure (0/0)
