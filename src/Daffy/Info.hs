-- | Runtime system info.

module Daffy.Info
  ( Info(..)
  , parseInfo
  ) where

import Daffy.Exception

import Control.Exception
import Data.Aeson (ToJSON(toJSON))
import GHC.Generics (Generic)
import System.Process (readProcess)
import Text.Read (readMaybe)

-- | Info about the runtime.
data Info = Info
  { threaded :: !Bool            -- ^ Is the runtime threaded?
  , feature  :: !(Maybe Feature) -- ^ Profiling, event log, debug, or none?
  } deriving (Generic)

instance ToJSON Info

data Feature
  = Profiling
  | EventLog
  | Debug

instance ToJSON Feature where
  toJSON = \case
    Profiling -> "profiling"
    EventLog -> "eventlog"
    Debug -> "debug"

-- | Parse an 'Info' from the output of './prog +RTS --info'.
parseInfo :: String -> IO Info
parseInfo prog = do
  output <- readProcess prog ["+RTS", "--info"] ""

  case readMaybe output :: Maybe [(String, String)] of
    Nothing -> throwIO (DaffyInfoParseException output)
    Just info ->
      case lookup "RTS way" info of
        Just "rts_v"         -> pure (Info False Nothing)
        Just "rts_p"         -> pure (Info False (Just Profiling))
        Just "rts_l"         -> pure (Info False (Just EventLog))
        Just "rts_debug"     -> pure (Info False (Just Debug))
        Just "rts_thr"       -> pure (Info True Nothing)
        Just "rts_thr_p"     -> pure (Info True (Just Profiling))
        Just "rts_thr_l"     -> pure (Info True (Just EventLog))
        Just "rts_thr_debug" -> pure (Info True (Just Debug))
        _                    -> throwIO (DaffyInfoParseException output)
