-- | Parsing and displaying @+RTS -pj@ profiles.

module Daffy.Profile
  ( ProfFile(..)
  , CostCenter(..)
  , Profile(..)
  , parse
  , allocFlamegraph
  , ticksFlamegraph
  ) where

import Data.Aeson
  (FromJSON, Value, defaultOptions, eitherDecode', fieldLabelModifier,
    genericParseJSON, parseJSON)
import Data.Aeson.Types (Parser)

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector as Vector

data ProfFile = ProfFile
  { program :: !Text
  , arguments :: !(Vector Text)
  , rts_arguments :: !(Vector Text)
  , end_time :: !Text
  , initial_capabilities :: !Int
  , total_time :: !Double
  , total_ticks :: !Int
  , tick_interval :: !Int
  , total_alloc :: !Integer
  , cost_centres :: !(Vector CostCenter)
  , profile :: !Profile
  } deriving (Generic, Show)

instance FromJSON ProfFile

data CostCenter = CostCenter
  { id :: !Int
  , label :: !Text
  , module_ :: !Text
  , src_loc :: !Text
  , is_caf :: !Bool
  } deriving (Generic, Show)

instance FromJSON CostCenter where
  parseJSON :: Value -> Parser CostCenter
  parseJSON =
    genericParseJSON
      (defaultOptions { fieldLabelModifier = dropTrailingUnderscore })
   where
    dropTrailingUnderscore :: [Char] -> [Char]
    dropTrailingUnderscore = \case
      [] ->
        []
      ['_'] ->
        []
      x:xs ->
        x : dropTrailingUnderscore xs

data Profile = Profile
  { id :: !Int
  , entries :: !Int
  , alloc :: !Int
  , ticks :: !Int
  , children :: !(Vector Profile)
  } deriving (Generic, Show)

instance FromJSON Profile

-- | Parse a @.prof@ file generated by @+RTS -pj@.
parse :: LByteString -> Either [Char] ProfFile
parse =
  eitherDecode'

-- | Fold a parsed 'ProfFile' into alloc lines suitable for input to
-- @flamegraph.pl@.
allocFlamegraph :: ProfFile -> [Text]
allocFlamegraph =
  flamegraph alloc

-- | Fold a parsed 'ProfFile' into ticks lines suitable for input to
-- @flamegraph.pl@.
ticksFlamegraph :: ProfFile -> [Text]
ticksFlamegraph =
  flamegraph ticks

flamegraph :: (Profile -> Int) -> ProfFile -> [Text]
flamegraph f prof =
  let
    go :: Profile -> [Text]
    go p =
      case n of
        0 ->
          ss
        _ ->
          name <> " " <> pack (show (f p)) : ss
     where
      n :: Int
      n =
        f p

      ss :: [Text]
      ss =
        map (\s -> name <> ";" <> s) (foldMap go (children p))

      name :: Text
      name =
        names IntMap.! (id :: Profile -> Int) p
  in
    go (profile prof)

 where
  names :: IntMap Text
  names =
    makeNames prof

makeNames :: ProfFile -> IntMap Text
makeNames prof =
  Vector.foldl' step mempty (cost_centres prof)
 where
  step :: IntMap Text -> CostCenter -> IntMap Text
  step acc CostCenter{id, label, module_, src_loc} =
    IntMap.insert id (module_ <> "." <> label <> ":" <> src_loc) acc
