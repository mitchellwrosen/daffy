import ElmCodegen

import Daffy.Stats
  (GCStats, GenStats, ParallelGCStats, SparksStats, Stats, TasksStats, Time)
import Data.Proxy
import Generics.SOP

main :: IO ()
main = do
  putStrLn (elmType (Proxy :: Proxy GCStats))
  putStrLn (elmType (Proxy :: Proxy GenStats))
  putStrLn (elmType (Proxy :: Proxy ParallelGCStats))
  putStrLn (elmType (Proxy :: Proxy SparksStats))
  putStrLn (elmType (Proxy :: Proxy Stats))
  putStrLn (elmType (Proxy :: Proxy TasksStats))
  putStrLn (elmType (Proxy :: Proxy Time))

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
