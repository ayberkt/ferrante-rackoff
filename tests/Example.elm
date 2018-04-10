module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Syntax exposing (Prop(..), RatPred(..), VarIdentifier(..), Expr(..))
import NNF exposing (convertToNNF)
import OmitNegations exposing (removeAllNegations)


suite : Test
suite =
    describe "Test suite"
        [ describe "Negation-normal form"
            [ test "NNF 1" <|
                \() ->
                    Expect.equal
                        (convertToNNF (Neg (Conj Top Top)))
                        (Disj (Neg Top) (Neg Top))
            , test "NNF 2" <|
                \() ->
                    Expect.equal
                        (convertToNNF (Neg (Disj Top Bot)))
                        (Conj (Neg Top) (Neg Bot))
            , test "NNF 3" <|
                \() ->
                    Expect.equal
                        (convertToNNF (Forall (VI "x") (Neg (Conj (Disj Bot Top) Top))))
                        (Forall
                            (VI "x")
                            (Disj (Conj (Neg Bot) (Neg Top)) (Neg Top))
                        )
            , test "Omit negations 1" <|
                \() ->
                    Expect.equal
                        (removeAllNegations
                            (convertToNNF
                                (Forall (VI "x")
                                    (Neg (Conj (Disj Bot Top) Top))
                                )
                            )
                        )
                        (Forall
                            (VI "x")
                            (Disj (Conj Top Bot) Bot)
                        )
            , test "Omit negations, pred 1" <|
                \() ->
                    Expect.equal
                        (removeAllNegations
                            (Neg (Pred (Less (Plus One One) (Plus Zero One))))
                        )
                        (Disj
                            (Pred (Greater (Plus Zero One) (Plus One One)))
                            (Pred (Eq (Plus One One) (Plus Zero One)))
                        )
            ]
        ]
