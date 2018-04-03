module Main exposing (..)

import Html exposing (Html, Attribute, beginnerProgram, text, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String
import Syntax exposing (Prop(..), linearize)
import PropositionParser exposing (parseProp)
import Styles exposing (..)
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
                div [ divStyle ]
                    [ Html.h1 [ heading ] [ text "QE for Linear Rationals" ]
                    , input
                        [ placeholder "Enter a proposition.", onInput NewContent, myStyle ]
                        []
                    , div [ waiting ] [ text ("Waiting for valid input...") ]
                    ]

            Ok p ->
                div [ divStyle ]
                    [ Html.h1 [ heading ] [ text "QE for Linear Rationals" ]
                    , input
                        [ placeholder "Enter a proposition."
                        , onInput NewContent
                        , myStyle
                        ]
                        []
                    , div [ myStyle ] [ text (inputInterpretation parse) ]
                    , div [ myStyle ] [ text (displayNNF parse) ]
                    , div [ myStyle ] [ text (displayAllNegationsRemoved parse) ]
                    ]
