module Daffy.List.Extra exposing (..)

import List.Nonempty as Nonempty exposing (Nonempty(Nonempty))


or : List Bool -> Bool
or =
    List.foldl (||) False


cons : a -> List a -> List a
cons =
    (::)


groupBy : (a -> a -> Bool) -> List a -> List (Nonempty a)
groupBy f =
    List.foldr
        (\x ys ->
            case ys of
                (Nonempty z zs) :: groups ->
                    if f x z then
                        Nonempty x (z :: zs) :: groups
                    else
                        Nonempty x [] :: ys

                [] ->
                    [ Nonempty x [] ]
        )
        []


unless : Bool -> a -> List a
unless b x =
    if b then
        []
    else
        [ x ]


when : Bool -> a -> List a
when b x =
    if b then
        [ x ]
    else
        []
