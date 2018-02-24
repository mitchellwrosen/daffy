module Daffy.List.Extra exposing (..)

import List.Nonempty as Nonempty exposing (Nonempty(Nonempty))


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
