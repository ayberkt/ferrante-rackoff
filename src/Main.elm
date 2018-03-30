import Html exposing (Html, Attribute, beginnerProgram, text, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String
import Syntax exposing (Prop(..), linearize)


main =
  beginnerProgram { model = "", view = view, update = update }


-- UPDATE

type Msg = NewContent String

update (NewContent content) oldContent =
  content


-- VIEW

inputInterpretation s = "Input interpretation: " ++ s

view content =
  div []
    [ input [ placeholder "Enter a proposition.", onInput NewContent, myStyle ] []
    , div [ myStyle ] [ text (inputInterpretation content) ]
    , div [ myStyle ] [ text (linearize (Conj Top Bot)) ]
    ]

myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]
