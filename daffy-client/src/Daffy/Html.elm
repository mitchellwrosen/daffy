module Daffy.Html exposing (..)

import Html exposing (..)
import Html.Attributes
import Html.Events


textInput : String -> (String -> a) -> List (Attribute a) -> Html a
textInput value message attributes =
    input
        (Html.Attributes.type_ "text"
            :: Html.Attributes.value value
            :: Html.Events.onInput message
            :: attributes
        )
        []


checkbox : String -> Bool -> a -> List (Attribute a) -> List (Html a)
checkbox value checked message attributes =
    [ input
        (Html.Attributes.type_ "checkbox"
            :: Html.Attributes.checked checked
            :: Html.Events.onCheck (\_ -> message)
            :: attributes
        )
        []
    , text value
    ]


radio : String -> Bool -> a -> List (Html a)
radio value isChecked msg =
    let
        id =
            "radio-" ++ value
    in
        [ input
            ([ Html.Attributes.type_ "radio"
             , Html.Attributes.checked isChecked
             , Html.Attributes.id id
             ]
                ++ if isChecked then
                    []
                   else
                    [ Html.Events.onClick msg ]
            )
            []
        , label [ Html.Attributes.for id ]
            [ text value
            ]
        ]
