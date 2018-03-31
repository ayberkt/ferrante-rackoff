module Main exposing (..)

import Html exposing (Html, Attribute, beginnerProgram, text, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String
import Syntax exposing (Prop(..), linearize)
import PropositionParser exposing (parseProp)
import NNF exposing (convertToNNF)


main =
    beginnerProgram { model = "", view = view, update = update }



-- UPDATE


type Msg
    = NewContent String


update (NewContent content) oldContent =
    content



-- VIEW


inputInterpretation p =
    case p of
        Err s ->
            "could not parse input"

        Ok p ->
            "Input interpretation: " ++ (linearize p) ++ "."


displayNNF p =
    case p of
        Err s ->
            "waiting for valid input"

        Ok p ->
            "NNF: " ++ (linearize (convertToNNF p)) ++ "."


view content =
    let
        parse =
            parseProp content
    in
        case parse of
            Err s ->
                div []
                    [ Html.h1 [ heading ] [ text "QE for Linear Rationals" ]
                    , input
                        [ placeholder "Enter a proposition.", onInput NewContent, myStyle ]
                        []
                    , div [ waiting ] [ text ("Waiting for valid input...") ]
                    ]

            Ok p ->
                div []
                    [ Html.h1 [ heading ] [ text "QE for Linear Rationals" ]
                    , input
                        [ placeholder "Enter a proposition."
                        , onInput NewContent
                        , myStyle
                        ]
                        []
                    , div [ myStyle ] [ text (inputInterpretation parse) ]
                    , div [ myStyle ] [ text (displayNNF parse) ]
                    ]


myStyle =
    style
        [ ( "width", "80%" )
        , ( "height", "40px" )
        , ( "padding", "10px 0" )
        , ( "font-size", "2em" )
        , ( "text-align", "center" )
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
