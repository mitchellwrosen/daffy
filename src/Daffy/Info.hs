-- | Runtime system info.

module Daffy.Info
  ( Info(..)
  , parseInfo
  ) where

import Daffy.Exception

import Data.Aeson (ToJSON(toJSON))
import System.Process (readProcess)

-- | Info about the runtime.
data Info = Info
  { threaded :: !Bool    -- ^ Is the runtime threaded?
  , feature  :: !Feature -- ^ Vanilla, profiling, event log, or debug?
  } deriving (Generic)

instance ToJSON Info

data Feature
  = Vanilla
  | Profiling
  | EventLog
  | Debug

instance ToJSON Feature where
  toJSON = \case
    Vanilla -> "vanilla"
    Profiling -> "profiling"
    EventLog -> "eventlog"
    Debug -> "debug"

-- | Parse an 'Info' from the output of './prog +RTS --info'.
parseInfo :: String -> IO Info
parseInfo prog = do
  output :: String <-
    readProcess prog ["+RTS", "--info"] ""

  case readMaybe output :: Maybe [(String, String)] of
    Nothing -> throw (DaffyInfoParseException output)
    Just info ->
      case lookup "RTS way" info of
        Just "rts_v"         -> pure (Info False Vanilla)
        Just "rts_p"         -> pure (Info False Profiling)
        Just "rts_l"         -> pure (Info False EventLog)
        Just "rts_debug"     -> pure (Info False Debug)
        Just "rts_thr"       -> pure (Info True Vanilla)
        Just "rts_thr_p"     -> pure (Info True Profiling)
        Just "rts_thr_l"     -> pure (Info True EventLog)
        Just "rts_thr_debug" -> pure (Info True Debug)
        _                    -> throw (DaffyInfoParseException output)
