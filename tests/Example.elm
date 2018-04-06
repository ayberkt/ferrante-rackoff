module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Syntax exposing (Prop(..))


suite : Test
suite =
    describe "Sample Test Suite"
        [ describe "Unit test examples"
            [ test "Addition" <| \() -> Expect.equal (3 + 7) 10 ]
        ]
