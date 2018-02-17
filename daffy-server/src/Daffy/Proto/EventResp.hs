{-# options_ghc -fno-warn-orphans #-}

module Daffy.Proto.EventResp
  ( EventResp(..)
  ) where

import Data.Aeson (ToJSON)
import GHC.RTS.Events
  (CapsetType(..), Event(..), EventInfo(..), HeapProfBreakdown(..),
    HeapProfFlags(..), KernelThreadId(..), MessageTag(..), ThreadStopStatus(..))

data EventResp = EventResp
  { type_ :: !Text -- ^ @"event"@.
  , event :: !Event
  } deriving (Generic)

instance ToJSON EventResp

-- Evil orphan instances... what to do about this...

deriving instance Generic CapsetType
deriving instance Generic Event
deriving instance Generic EventInfo
deriving instance Generic HeapProfBreakdown
deriving instance Generic HeapProfFlags
deriving instance Generic KernelThreadId
deriving instance Generic MessageTag
deriving instance Generic ThreadStopStatus

instance ToJSON CapsetType
instance ToJSON Event
instance ToJSON EventInfo
instance ToJSON HeapProfBreakdown
instance ToJSON HeapProfFlags
instance ToJSON KernelThreadId
instance ToJSON MessageTag
instance ToJSON ThreadStopStatus
