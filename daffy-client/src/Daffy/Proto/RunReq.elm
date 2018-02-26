module Daffy.Proto.RunReq exposing (..)

{-| A request to run a command.

This module corresponds to:

     daffy-server/src/Daffy/Proto/RunReq.hs

-}

import Json.Encode as Json exposing (bool, object, string)


{-| A request to run a command
-}
type alias RunReq =
    { command : String
    , stats : Bool
    , prof : Bool
    , eventlog : Bool
    }


{-| Encode a `RunReq` as JSON.
-}
encode : RunReq -> String
encode request =
    [ ( "command", string request.command )
    , ( "stats", bool request.stats )
    , ( "prof", bool request.prof )
    , ( "eventlog", bool request.eventlog )
    ]
        |> object
        |> Json.encode 0
