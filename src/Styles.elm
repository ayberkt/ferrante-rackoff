module Styles exposing (divStyle, myStyle, heading, waiting)

import Html exposing (Html, Attribute, beginnerProgram, text, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


divStyle =
    style
        [ ( "text-align", "center" )
        , ( "align-content", "center" )
        , ( "margin", "0 auto" )
        ]


myStyle =
    style
        [ ( "width", "50%" )
        , ( "height", "30px" )
        , ( "padding", "10px 0" )
        , ( "font-size", "2em" )
        , ( "text-align", "center" )
        , ( "align-content", "center" )
        , ( "margin", "0 auto" )
        ]


heading =
    style [ ( "text-align", "center" ) ]


waiting =
    style
        [ ( "width", "100%" )
        , ( "height", "40px" )
        , ( "padding", "10px 0" )
        , ( "font-size", "2em" )
        , ( "text-align", "center" )
        , ( "color", "red" )
        ]
