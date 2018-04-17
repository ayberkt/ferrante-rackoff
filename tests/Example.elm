module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Syntax exposing (Prop(..), RatPred(..), VarIdentifier(..), Expr(..), Rat(..))
import NNF exposing (convertToNNF)
import OmitNegations exposing (removeAllNegations)
import Solve exposing (solve)
import PropositionParser exposing (parseProp)


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
            , test "Omit negations 2" <|
                \() ->
                    Expect.equal
                        (removeAllNegations
                            (convertToNNF
                                (Forall (VI "x")
                                    (Conj (Disj Bot Top) Top)
                                )
                            )
                        )
                        (Forall
                            (VI "x")
                            (Conj (Disj Bot Top) Top)
                        )
            , test "Omit negations 3" <|
                \() ->
                    Expect.equal
                        (removeAllNegations
                            (convertToNNF
                                (Forall (VI "x")
                                    (Neg (Pred (Less (Var 0 (VI "x")) (Var 0 (VI "x")))))
                                )
                            )
                        )
                        (Forall
                            (VI "x")
                            (Disj
                                (Pred (Greater (Var 0 (VI "x")) (Var 0 (VI "x"))))
                                (Pred (Eq (Var 0 (VI "x")) (Var 0 (VI "x"))))
                            )
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
            , test "Omit negations, pred 2" <|
                \() ->
                    Expect.equal
                        (removeAllNegations
                            (Neg (Pred (Less Zero One)))
                        )
                        (Disj
                            (Pred (Greater One Zero))
                            (Pred (Eq Zero One))
                        )
            , test "Omit negations, pred 3" <|
                \() ->
                    Expect.equal
                        (removeAllNegations
                            (Neg (Pred (Less Zero One)))
                        )
                        (Disj
                            (Pred (Greater One Zero))
                            (Pred (Eq Zero One))
                        )
            , test "Omit negations, pred 4" <|
                \() ->
                    Expect.equal
                        (removeAllNegations Top)
                        Top
            , test "Solve constant, 1" <|
                \() ->
                    Expect.equal
                        (solve
                            (Forall (VI "x")
                                (Pred
                                    (Less
                                        (ConstFact (Div 3 1) (Var 0 (VI "x")))
                                        (ConstFact (Div 2 1) (Var 0 (VI "x")))
                                    )
                                )
                            )
                        )
                        (Forall (VI "x")
                            (Pred
                                (Less
                                    (ConstFact
                                        (Div 1 2)
                                        (ConstFact (Div 3 1) (Var 0 (VI "x")))
                                    )
                                    (Var 0 (VI "x"))
                                )
                            )
                        )
            , test "Solve constant, 2" <|
                \() ->
                    Expect.equal
                        (solve
                            (Forall (VI "x")
                                (Pred
                                    (Less
                                        (ConstFact (Div 1 3) (Var 0 (VI "x")))
                                        (ConstFact (Div 1 10) (Var 0 (VI "x")))
                                    )
                                )
                            )
                        )
                        (Forall (VI "x")
                            (Pred
                                (Less
                                    (ConstFact
                                        (Div 10 1)
                                        (ConstFact (Div 1 3) (Var 0 (VI "x")))
                                    )
                                    (Var 0 (VI "x"))
                                )
                            )
                        )
            , test "Solve constant, 3" <|
                \() ->
                    Expect.equal
                        (solve
                            (Pred
                                (Less (ConstFact (Div 3 1) One)
                                    (ConstFact (Div 2 1) One)
                                )
                            )
                        )
                        (Pred
                            (Less
                                (ConstFact (Div 1 2) (ConstFact (Div 3 1) One))
                                One
                            )
                        )
            , test "Solve constant, 4" <|
                \() ->
                    Expect.equal
                        (solve
                            (Pred
                                (Eq (ConstFact (Div 3 1) One)
                                    (ConstFact (Div 2 1) One)
                                )
                            )
                        )
                        (Pred
                            (Eq
                                (ConstFact (Div 1 2) (ConstFact (Div 3 1) One))
                                One
                            )
                        )
            , test "Solve constant, 5" <|
                \() ->
                    Expect.equal
                        (solve
                            (Pred
                                (Less
                                    (ConstFact (Div 2 1) One)
                                    (ConstFact (Div 3 1) One)
                                )
                            )
                        )
                        (Pred
                            (Less (ConstFact (Div 1 3) (ConstFact (Div 2 1) One)) One)
                        )
            , test "Solve constant, 6" <|
                \() ->
                    Expect.equal (solve (Pred (Less One One))) (Pred (Less One One))
            , test "Parser 1" <|
                \() ->
                    Expect.equal
                        (parseProp "true")
                        (Just Top)
            , test "Parser 2" <|
                \() ->
                    Expect.equal
                        (parseProp "(/\\ true true)")
                        (Just (Conj Top Top))
            , test "Parser 3" <|
                \() ->
                    Expect.equal
                        (parseProp "(\\/ true false)")
                        (Just (Disj Top Bot))
            , test "Parser 4" <|
                \() ->
                    Expect.equal
                        (parseProp "(~ (forall x (< x x)))")
                        (Just (Neg (Forall (VI "x") (Pred (Less (Var 0 (VI "x")) (Var 0 (VI "x")))))))
            , test "Parser 5" <|
                \() ->
                    Expect.equal
                        (parseProp "(< (+ 1 1) (+ 0 1))")
                        (Just (Pred (Less (Plus One One) (Plus Zero One))))
            , test "Parser 6" <|
                \() ->
                    Expect.equal
                        (parseProp "(forall x (> x x))")
                        (Just (Forall (VI "x") (Pred (Greater (Var 0 (VI "x")) (Var 0 (VI "x"))))))
            , test "Parser 7" <|
                \() ->
                    Expect.equal
                        (parseProp "(forall x (< x x))")
                        (Just (Forall (VI "x") (Pred (Less (Var 0 (VI "x")) (Var 0 (VI "x"))))))
            , test "Parser 8" <|
                \() ->
                    Expect.equal
                        (parseProp "(~ (forall x (< x x)")
                        Nothing
            , test "Parser 9" <|
                \() ->
                    Expect.equal
                        (parseProp "(forall x (< (* 3 x) (* 2 x)))")
                        (Just
                            (Forall (VI "x")
                                (Pred
                                    (Less
                                        (ConstFact (Div 3 1) (Var 0 (VI "x")))
                                        (ConstFact (Div 2 1) (Var 0 (VI "x")))
                                    )
                                )
                            )
                        )
            , test "Parser 10" <|
                \() ->
                    Expect.equal
                        (parseProp "(forall x (< (* 3 x) 1))")
                        (Just
                            (Forall (VI "x")
                                (Pred
                                    (Less
                                        (ConstFact (Div 3 1) (Var 0 (VI "x")))
                                        One
                                    )
                                )
                            )
                        )
            ]
        ]
