module Daffy.Lenses exposing (..)


overRunSpec : (a -> a) -> { r | runSpec : a } -> { r | runSpec : a }
overRunSpec f x =
    { x | runSpec = f x.runSpec }


setCommand : a -> { r | command : a } -> { r | command : a }
setCommand x s =
    { s | command = x }


setCompaction : a -> { r | compaction : a } -> { r | compaction : a }
setCompaction x s =
    { s | compaction = x }


setEventlog : a -> { r | eventlog : a } -> { r | eventlog : a }
setEventlog x s =
    { s | eventlog = x }


setLargeObjectSize : a -> { r | largeObjectSize : a } -> { r | largeObjectSize : a }
setLargeObjectSize x s =
    { s | largeObjectSize = x }


setOldGenFactor : a -> { r | oldGenFactor : a } -> { r | oldGenFactor : a }
setOldGenFactor x s =
    { s | oldGenFactor = x }


setOldGenMinSize : a -> { r | oldGenMinSize : a } -> { r | oldGenMinSize : a }
setOldGenMinSize x s =
    { s | oldGenMinSize = x }


setProf : a -> { r | prof : a } -> { r | prof : a }
setProf x s =
    { s | prof = x }


setNurseryChunks : a -> { r | nurseryChunks : a } -> { r | nurseryChunks : a }
setNurseryChunks x s =
    { s | nurseryChunks = x }


setNurserySize : a -> { r | nurserySize : a } -> { r | nurserySize : a }
setNurserySize x s =
    { s | nurserySize = x }


setStats : a -> { r | stats : a } -> { r | stats : a }
setStats x s =
    { s | stats = x }
