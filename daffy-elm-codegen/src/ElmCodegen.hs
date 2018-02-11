{-# language DataKinds            #-}
{-# language GADTs                #-}
{-# language FlexibleContexts     #-}
{-# language FlexibleInstances    #-}
{-# language ScopedTypeVariables  #-}
{-# language LambdaCase           #-}
{-# language UndecidableInstances #-}

module ElmCodegen
  ( ElmType(..)
  , elmType
  ) where

import Data.List (intercalate)
import Generics.SOP

import qualified GHC.Generics as GHC

class ElmType a where
  elmTypeName :: proxy a -> [Char]

instance ElmType Bool where
  elmTypeName _ =
    "Bool"

instance ElmType Double where
  elmTypeName _ =
    "Float"

instance ElmType Int where
  elmTypeName _ =
    "Int"

instance ElmType a => ElmType [a] where
  elmTypeName _ =
    "List " ++ elmTypeName (Proxy :: Proxy a)

instance ElmType a => ElmType (Maybe a) where
  elmTypeName _ =
    "Maybe " ++ elmTypeName (Proxy :: Proxy a)

-- Catch-all instance: use the datatype's name
instance
    {-# OVERLAPPABLE #-}
    (GHC.Datatype c, GHC.Generic a, GHC.Rep a ~ GHC.M1 GHC.D c f) =>
    ElmType a where
  elmTypeName _ =
    GHC.datatypeName (GHC.from (undefined :: a))

elmType
  :: forall a xs.
     (All ElmType xs, Code a ~ '[xs], HasDatatypeInfo a, Generic a)
  => Proxy a -> [Char]
elmType p =
  case datatypeInfo p of
    ADT _ name (Record _ info :* Nil) ->
      "type alias " ++ name ++ " =\n    { "
        ++ intercalate "\n    , " (elmType' info) ++ "\n    }"
    _ ->
      error "Not a record"

elmType' :: All ElmType xs => NP FieldInfo xs -> [[Char]]
elmType' = \case
  Nil ->
    []
  info@(FieldInfo name) :* infos ->
    (name ++ " : " ++ elmTypeName info) : elmType' infos
