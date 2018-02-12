module Daffy.Command
  ( Command(..)
  , render
  ) where

import Data.Aeson (FromJSON, ToJSON)

data Command = Command
  { command :: [Char] -- ^ Raw shell command. Invariant: non-empty.
  , stats :: Bool -- ^ Generate runtime stats?
  , eventlog :: Bool -- ^ Write an eventlog?
  , prof :: Bool -- ^ Generate a time and allocation profile?
  } deriving (Generic)

instance FromJSON Command
instance ToJSON Command

-- | Render a 'Command' as a full shell command including all RTS opts.
render :: FilePath -> FilePath -> Command -> [Char]
render proffile statsfile cmd =
  concat
    [ command cmd
    , " +RTS"
    , if eventlog cmd
        then " -l"
        else ""
    , if prof cmd
        then " -pj -po" ++ proffile
        else ""
    , if stats cmd
        then " -S" ++ statsfile
        else ""
    ]
