{-# language ConstraintKinds      #-}
{-# language DataKinds            #-}
{-# language FlexibleContexts     #-}
{-# language FlexibleInstances    #-}
{-# language GADTs                #-}
{-# language LambdaCase           #-}
{-# language RankNTypes           #-}
{-# language ScopedTypeVariables  #-}
{-# language UndecidableInstances #-}

module ElmCodegen
  ( ElmType(..)
  , elmType
  , elmDecoder
  ) where

import Data.Char (toLower)
import Data.List (intercalate)
import Generics.SOP

import qualified GHC.Generics as GHC

--------------------------------------------------------------------------------
-- Elm type generation

class ElmType a where
  renderElmType :: proxy a -> [Char]

instance ElmType Bool where
  renderElmType _ =
    "Bool"

instance ElmType Double where
  renderElmType _ =
    "Float"

instance ElmType Int where
  renderElmType _ =
    "Int"

instance ElmType a => ElmType [a] where
  renderElmType _ =
    "List " ++ renderElmType (Proxy :: Proxy a)

instance ElmType a => ElmType (Maybe a) where
  renderElmType _ =
    "Maybe " ++ renderElmType (Proxy :: Proxy a)

-- Catch-all instance: use the datatype's name
instance
    {-# OVERLAPPABLE #-}
    (GHC.Datatype c, GHC.Generic a, GHC.Rep a ~ GHC.M1 GHC.D c f) =>
    ElmType a where
  renderElmType _ =
    GHC.datatypeName (GHC.from (undefined :: a))

-- | Generate an Elm type alias from a Haskell record.
elmType
  :: (All ElmType xs, Code a ~ '[xs], HasDatatypeInfo a, Generic a)
  => Proxy a -> [Char]
elmType p =
  case datatypeInfo p of
    ADT _ name (Record _ info :* Nil) ->
      "type alias " ++ name ++ " =\n    { "
        ++ intercalate "\n    , " (elmType' info) ++ "\n    }\n"
    _ ->
      error "Not a record"

elmType' :: All ElmType xs => NP FieldInfo xs -> [[Char]]
elmType' = \case
  Nil ->
    []
  info@(FieldInfo name) :* infos ->
    (name ++ " : " ++ renderElmType info) : elmType' infos

--------------------------------------------------------------------------------
-- Elm JSON decoder generation

class ElmDecoder a where
  renderElmDecoder :: proxy a -> [Char]

instance ElmDecoder Bool where
  renderElmDecoder _ =
    "bool"

instance ElmDecoder Double where
  renderElmDecoder _ =
    "float"

instance ElmDecoder Int where
  renderElmDecoder _ =
    "int"

instance ElmDecoder a => ElmDecoder [a] where
  renderElmDecoder _ =
    "(list " ++ renderElmDecoder (Proxy :: Proxy a) ++ ")"

instance ElmDecoder a => ElmDecoder (Maybe a) where
  renderElmDecoder _ =
    "(nullable " ++ renderElmDecoder (Proxy :: Proxy a) ++ ")"

-- Catch-all instance: use the datatype's name
instance
    {-# OVERLAPPABLE #-}
    (GHC.Datatype c, GHC.Generic a, GHC.Rep a ~ GHC.M1 GHC.D c f) =>
    ElmDecoder a where
  renderElmDecoder _ =
    "decode" ++ GHC.datatypeName (GHC.from (undefined :: a))

-- | Generate an Elm JSON decoder from a Haskell record.
elmDecoder
  :: (All ElmDecoder xs, Code a ~ '[xs], HasDatatypeInfo a, Generic a)
  => Proxy a -> [Char]
elmDecoder p =
  case datatypeInfo p of
    ADT _ name (Record _ info :* Nil) ->
      unlines $
        [ lower1 name ++ "Decoder : Decoder " ++ name
        , lower1 name ++ "Decoder ="
        , "    decode" ++ name
        ] ++ map ("        |> " ++) (elmDecoder' info)
    _ ->
      error "Not a record"

elmDecoder' :: All ElmDecoder xs => NP FieldInfo xs -> [[Char]]
elmDecoder' = \case
  Nil ->
    []
  info@(FieldInfo name) :* infos ->
    ("required \"" ++ name ++ "\" " ++ renderElmDecoder info)
      : elmDecoder' infos

lower1 :: [Char] -> [Char]
lower1 = \case
  [] ->
    []
  x:xs ->
    toLower x : xs
