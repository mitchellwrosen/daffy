-- | Runtime system info.

module Daffy.Info
  ( Info(..)
  , Feature(..)
  , run
  ) where

import Daffy.Exception

import Data.Aeson (ToJSON(toJSON))
import System.Process.Typed

import qualified Data.ByteString.Lazy.Char8 as LChar8

-- | Info about the runtime.
data Info = Info
  { threaded :: !Bool    -- ^ Is the runtime threaded?
  , feature  :: !Feature -- ^ Vanilla, profiling, event log, or debug?
  } deriving (Eq, Generic, Show)

instance ToJSON Info

data Feature
  = Vanilla
  | Profiling
  | EventLog
  | Debug
  deriving (Eq, Show)

instance ToJSON Feature where
  toJSON = \case
    Vanilla -> "vanilla"
    Profiling -> "profiling"
    EventLog -> "eventlog"
    Debug -> "debug"

parse :: LByteString -> Maybe Info
parse bytes = do
  info :: [([Char], [Char])] <-
    readMaybe (LChar8.unpack bytes)
  lookup "RTS way" info >>= \case
    "rts_v" -> Just (Info False Vanilla)
    "rts_p" -> Just (Info False Profiling)
    "rts_l" -> Just (Info False EventLog)
    "rts_debug" -> Just (Info False Debug)
    "rts_thr" -> Just (Info True Vanilla)
    "rts_thr_p" -> Just (Info True Profiling)
    "rts_thr_l" -> Just (Info True EventLog)
    "rts_thr_debug" -> Just (Info True Debug)
    _ -> Nothing

-- | Parse an 'Info' from the output of './prog +RTS --info'. If the program
-- exits with non-zero exit code, or parsing fails, throws
-- 'InfoParseException'.
run :: [Char] -> IO Info
run cmd = do
  (code, out, _) :: (ExitCode, LByteString, LByteString) <-
    readProcess (shell (cmd ++ " +RTS --info"))

  let minfo :: Maybe Info
      minfo = do
        ExitSuccess <- pure code
        parse out

  case minfo of
    Nothing ->
      throw (InfoParseException out)
    Just info ->
      pure info
