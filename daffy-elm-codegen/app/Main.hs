{-# OPTIONS_GHC -fno-warn-orphans #-}

import ElmCodegen

import Daffy.Stats
  (GCStats, GenStats, ParallelGCStats, SparksStats, Stats, TasksStats, Time)
import Data.Proxy
import Generics.SOP

main :: IO ()
main =
  mapM_ putStrLn
    [ "module DaffyTypes exposing (..)"
    , ""
    , "import Json.Decode exposing (..)"
    , "import Json.Decode.Pipeline exposing (..)"
    , ""
    , elmType (Proxy :: Proxy GCStats)
    , elmType (Proxy :: Proxy GenStats)
    , elmType (Proxy :: Proxy ParallelGCStats)
    , elmType (Proxy :: Proxy SparksStats)
    , elmType (Proxy :: Proxy Stats)
    , elmType (Proxy :: Proxy TasksStats)
    , elmType (Proxy :: Proxy Time)
    , elmDecoder (Proxy :: Proxy GCStats)
    , elmDecoder (Proxy :: Proxy GenStats)
    , elmDecoder (Proxy :: Proxy ParallelGCStats)
    , elmDecoder (Proxy :: Proxy SparksStats)
    , elmDecoder (Proxy :: Proxy Stats)
    , elmDecoder (Proxy :: Proxy TasksStats)
    , elmDecoder (Proxy :: Proxy Time)
    ]

instance Generic GCStats
instance Generic GenStats
instance Generic ParallelGCStats
instance Generic SparksStats
instance Generic Stats
instance Generic TasksStats
instance Generic Time

instance HasDatatypeInfo GCStats
instance HasDatatypeInfo GenStats
instance HasDatatypeInfo ParallelGCStats
instance HasDatatypeInfo SparksStats
instance HasDatatypeInfo Stats
instance HasDatatypeInfo TasksStats
instance HasDatatypeInfo Time
