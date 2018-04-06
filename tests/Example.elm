module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Syntax exposing (Prop(..))
import NNF exposing (convertToNNF)


suite : Test
suite =
    describe "Test suite"
        [ describe "Negation-normal form"
            [ test "NNF 1" <|
                \() ->
                    Expect.equal
                        (convertToNNF (Neg (Conj Top Top)))
                        (Disj (Neg Top) (Neg Top))
            ]
        ]
