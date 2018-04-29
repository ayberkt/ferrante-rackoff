module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href, class, style, rel)
import Material
import Material.Scheme
import Material.Button as Button
import Material.Grid exposing (..)
import Material.Options as Options exposing (css)
import Material.Textfield as Textfield
import Material.Color as Color
import Material.List as L
import NNF exposing (convertToNNF)
import Syntax exposing (..)
import PropositionParser exposing (parseProp)
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
    , input     : Input
    , mdl       : Material.Model
    }


model : Model
model =
    { count     = 0
    , input     = InvalidInput
    , inputText = ""
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
    , case model.input of
        InvalidInput ->
          Options.div
            []
            [ Options.styled Html.body
              [ css "font-size" "20px", Color.text Color.accent ]
              [ text "Waiting for valid input." ] ]
        Parsed p ->
          Options.div
          []
          [ subTitle "Negation-normal form"
          , Options.styled
            Html.body
            [css "font-size" "20px" ]
            [ text (linearize (convertToNNF p)) ]
          , subTitle "No negations"
          , subTitle "Simplified"
          ]
    ]
    |> Material.Scheme.top



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
