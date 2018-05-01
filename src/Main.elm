module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href, class, style, rel)
import Material
import Material.Scheme
import Material.Button as Button
import Material.Grid exposing (..)
import Material.Options as Options exposing (css)
import Material.Textfield as Textfield
import Material.Toggles   as Toggles
import Material.Slider   as Slider
import Material.Color as Color
import Material.List as L
import NNF exposing (convertToNNF)
import OmitNegations exposing (removeAllNegations)
import Solve exposing (solve)
import Syntax exposing (..)
import PropositionParser exposing (parseProp)
import InfiniteProjection
       exposing (leftInfProj, rightInfProj, constructF3, constructDisjunct)
import Normalization exposing (normalize)
import Satisfiability exposing (..)
import Debug exposing (log)
import Json.Encode
import Json.Decode



-- MODEL


type Input
    = InvalidInput
    | Parsed Prop


type alias Model =
    { count     : Int
    , inputText : String
    , fontSize  : Float
    , input     : Input
    , showSteps : Bool
    , mdl       : Material.Model
    }


model : Model
model =
    { count     = 0
    , input     = InvalidInput
    , inputText = ""
    , fontSize  = 20
    , showSteps = True
    , mdl       = Material.model
    }


boxed : List (Options.Property a b)
boxed =
    [ css "margin" "auto"
    , css "padding-left" "8%"
    , css "padding-right" "8%"
    ]



-- ACTION, UPDATE


type Msg
    = Increase
    | Reset
    | UpdateMessage String
    | ChangeFontSize Float
    | ToggleSteps
    | Mdl (Material.Msg Msg)



-- Boilerplate: Msg clause for internal Mdl messages.


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increase ->
            ( { model | count = model.count + 1 }
            , Cmd.none
            )

        Reset ->
            ( { model | count = 0 }
            , Cmd.none
            )

        UpdateMessage s ->
          let
              _ =  log "updating message"
          in
            case parseProp s of
              Just p  -> ( { model | input = Parsed p,     inputText = s }, Cmd.none )
              Nothing -> ( { model | input = InvalidInput, inputText = s }, Cmd.none )

        ToggleSteps -> ( { model | showSteps = not model.showSteps }, Cmd.none )

        ChangeFontSize f -> ( { model | fontSize = f }, Cmd.none)

        -- Boilerplate: Mdl action handler.
        Mdl msg_ ->
            Material.update Mdl msg_ model



-- VIEW


type alias Mdl =
    Material.Model


title : String -> Html a
title t =
    Options.styled Html.h1
        [ Color.text Color.accent ]
        [ text t ]


subTitle : String -> Html a
subTitle t =
    Options.styled Html.h2
        [ Color.text Color.primary ]
        [ text t ]


goodResult : String -> Html a
goodResult t =
  Options.styled Html.h2
    [ Color.text (Color.color Color.LightGreen Color.S800) ]
    [ text t ]

badResult : String -> Html a
badResult t =
  Options.styled Html.h2
    [ Color.text (Color.color Color.Red Color.S800) ]
    [ text t ]

view : Model -> Html Msg
view model =
  Options.div
  boxed
    [ title "QE for Linear Rationals"
    , Textfield.render Mdl
        [ 2 ]
        model.mdl
        [ Options.onInput UpdateMessage ]
        [ Textfield.label "Proposition" ]
    , Button.render Mdl
        [ 0 ]
        model.mdl
        [ Options.onClick Increase, css "margin" "0 24px" ]
        [ text "Start" ]
    , Toggles.switch Mdl [0] model.mdl
        [ Options.onToggle ToggleSteps
        , Toggles.ripple
        , Toggles.value model.showSteps
        ]
        [ text "Show steps" ]
    , Slider.view
      [ Slider.onChange ChangeFontSize
      , css "margin" "0 24px"
      , Slider.value model.fontSize]
    , (case model.input of
        InvalidInput ->
          Options.div
            []
            [ Options.styled Html.body
              [ css "font-size" "20px",
                Color.text Color.accent ]
              [ text "Waiting for valid input." ] ]
        Parsed p ->
          let
              existentials         = getExistentials p
              indices  = List.range 1 (List.length existentials)
          in
            Options.div
              []
              (List.concat
                (List.reverse
                  (List.map2
                    (\p i -> (genSimpleExplicationResult model p (toString i)))
                    existentials
                    indices))))
    ] |> Material.Scheme.top

genSimpleExplicationResult model r s =
  case r of
    Existential p -> genSimpleExplicationProp model p s
    NegatedExistential p -> genSimpleExplicationProp model p s
    NoExistentialFound -> [ ]

genSimpleExplicationProp model p s =
    let
        nnf            = convertToNNF p
        noNegs         = removeAllNegations nnf
        simplified     = solve noNegs
        (leftProj,  _) = leftInfProj simplified
        (rightProj, _) = rightInfProj simplified
        middleCases    = constructF3 simplified
        steps          =
          [ subTitle ("Negation-normal form  " ++ s)
          , Options.styled
            Html.body
            [css "font-size" ((toString model.fontSize) ++ "px") ]
            [ text (linearize (convertToNNF p)) ]
          , subTitle ("No negations  " ++ "(" ++ s ++ ")")
          , Options.styled
            Html.body
            [css "font-size" ((toString model.fontSize) ++ "px") ]
            [ text (linearize noNegs) ]
          , subTitle ("Simplified  " ++ "(" ++ s ++ ")")
          , Options.styled
            Html.body
            [css "font-size" ((toString model.fontSize) ++ "px") ]
            [ text (linearize simplified) ]
          , subTitle ("Left Infinite Projection" ++ "(" ++ s ++ ")")
          , Options.styled
            Html.body
            [css "font-size" ((toString model.fontSize) ++ "px") ]
            [ text (linearize leftProj) ]
          , subTitle ("Right Infinite Projection  " ++ "(" ++ s ++ ")")
          , Options.styled
            Html.body
            [css "font-size" ((toString model.fontSize) ++ "px") ]
            [ text (linearize rightProj) ]
          , subTitle ("Middle case  " ++ s)
          , Options.styled
            Html.body
            [css "font-size" ((toString model.fontSize) ++ "px") ]
            [
              text
                ((linearize (constructDisjunct middleCases))
                 ++ " which normalizes to "
                 ++ (toString (normalize (constructDisjunct middleCases))))
            ]
        ]
      in
        case isSat simplified of
          Conclusion True ->
            if model.showSteps then
              steps ++ [goodResult "Satisfiable"]
            else
              [goodResult "Satisfiable"]
          _ ->
            if model.showSteps then
              steps ++ [badResult "Not satisfiable"]
            else
              [badResult "Not satisfiable"]



-- Load Google Mdl CSS. You'll likely want to do that not in code as we
-- do here, but rather in your master .html file. See the documentation
-- for the `Material` module for details.


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, Cmd.none )
        , view = view
        , subscriptions = always Sub.none
        , update = update
        }
