module Main exposing (..)

import Html exposing (Html, Attribute, beginnerProgram, text, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String
import Syntax exposing (Prop(..), linearize)
import PropositionParser exposing (parseProp)
import Styles exposing (..)
import OmitNegations exposing (removeAllNegations)
import Solve exposing (solve)
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
    "Input interpretation: " ++ (linearize p) ++ "."


displayNNF p =
    "Negation-normal form: " ++ linearize (convertToNNF p) ++ "."


displayAllNegationsRemoved p =
    "No negations: " ++ (linearize (removeAllNegations (convertToNNF p))) ++ "."


displayConstantsSolved p =
    "Solved constants: "
        ++ linearize ((solve (removeAllNegations (convertToNNF p))))
        ++ "."



-- If the input is parsable, parse and display the steps.  If it is not
-- parsable, display an error explaining that it is not parsable.


view content =
    let
        parse =
            parseProp content
    in
        case parse of
            Nothing ->
                div [ divStyle ]
                    [ Html.h1 [ heading ] [ text "QE for Linear Rationals" ]
                    , input
                        [ placeholder "Enter a proposition.", onInput NewContent, myStyle ]
                        []
                    , div [ waiting ] [ text ("Waiting for valid input...") ]
                    ]

            Just parse ->
                div [ divStyle ]
                    [ Html.h1 [ heading ] [ text "QE for Linear Rationals" ]
                    , input
                        [ placeholder "Enter a proposition."
                        , onInput NewContent
                        , myStyle
                        ]
                        []
                    , Html.h2 [ heading ] [ text "Step 1" ]
                    , div [ myStyle ] [ text (inputInterpretation parse) ]
                    , Html.h2 [ heading ] [ text "Step 2" ]
                    , div [ myStyle ] [ text (displayNNF parse) ]
                    , Html.h2 [ heading ] [ text "Step 3" ]
                    , div [ myStyle ] [ text (displayAllNegationsRemoved parse) ]
                    , Html.h2 [ heading ] [ text "Step 4" ]
                    , div [ myStyle ] [ text (displayConstantsSolved parse) ]
                    ]
