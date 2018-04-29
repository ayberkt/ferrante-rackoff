module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Syntax exposing (Prop(..), RatPred(..), VarIdentifier(..), Expr(..), Rat(..))
import NNF exposing (convertToNNF)
import OmitNegations exposing (removeAllNegations)
import Solve exposing (solve)
import InfiniteProjection exposing (leftInfProj, rightInfProj, constructF3)
import Maybe exposing (withDefault)
import PropositionParser exposing (parseProp)
import Satisfiability exposing (isSat)
import Normalization exposing (normalizeExpr, normalizeRatPred, normalize)


injDiv : Int -> Int -> Expr
injDiv z1 z2 =
    ConstRat (Div z1 z2)


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
                        (convertToNNF
                          (Forall (VI "x") (Neg (Conj (Disj Bot Top) Top))))
                        (Neg (Exists
                            (VI "x")
                            (Conj (Disj Bot Top) Top)))
            , test "Omit negations 1" <|
                \() ->
                    Expect.equal
                        (removeAllNegations
                            (convertToNNF
                                (Forall (VI "x")
                                  (Neg (Conj (Disj Bot Top) Top)))))
                        (Neg (Exists (VI "x") (Conj (Disj Bot Top) Top)))
            , test "Omit negations 2" <|
                \() ->
                    Expect.equal
                        (removeAllNegations
                            (convertToNNF
                                (Forall (VI "x")
                                    (Conj (Disj Bot Top) Top))))
                        (Neg (Exists
                            (VI "x")
                            (Disj (Conj Top Bot) Bot)))
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
                        (Neg
                          (Exists
                            (VI "x")
                            (Pred (Less (Var 0 (VI "x")) (Var 0 (VI "x"))))))
            , test "Omit negations, pred 1" <|
                \() ->
                    Expect.equal
                        (removeAllNegations
                            (Neg
                                (Pred
                                    (Less
                                        (Plus (injDiv 1 1) (injDiv 1 1))
                                        (Plus (injDiv 0 1) (injDiv 1 1))))))
                        (removeAllNegations
                            (Neg
                                (Pred
                                    (Less
                                        (Plus (injDiv 1 1) (injDiv 1 1))
                                        (Plus (injDiv 0 1) (injDiv 1 1))))))
            , test "Omit negations, pred 2" <|
                \() ->
                  Expect.equal
                    (removeAllNegations
                      (Neg (Pred (Less (injDiv 0 1) (injDiv 1 1)))))
                    (Disj
                      (Pred (Greater (injDiv 0 1) (injDiv 1 1)))
                      (Pred (Eq (injDiv 0 1) (injDiv 1 1))))
            , test "Omit negations, pred 3" <|
                \() ->
                    Expect.equal
                        (removeAllNegations
                            (Neg (Pred (Less (injDiv 0 1) (injDiv 1 1))))
                        )
                        (Disj
                            (Pred (Greater (injDiv 0 1) (injDiv 1 1)))
                            (Pred (Eq (injDiv 0 1) (injDiv 1 1)))
                        )
            , test "Omit negations, pred 4" <|
                \() ->
                    Expect.equal
                        (removeAllNegations Top)
                        Top
            , test "Solve constant, 3" <|
                \() ->
                    Expect.equal
                        (solve
                            (Pred
                                (Less (ConstFact (Div 3 1) (injDiv 1 1))
                                    (ConstFact (Div 2 1) (injDiv 1 1))
                                )
                            )
                        )
                        (Pred
                            (Less
                                (ConstFact (Div 1 2) (ConstFact (Div 3 1) (injDiv 1 1)))
                                (injDiv 1 1)
                            )
                        )
            , test "Solve constant, 4" <|
                \() ->
                    Expect.equal
                        (solve
                            (Pred
                                (Eq (ConstFact (Div 3 1) (injDiv 1 1))
                                    (ConstFact (Div 2 1) (injDiv 1 1))
                                )
                            )
                        )
                        (Pred
                            (Eq
                                (ConstFact (Div 1 2) (ConstFact (Div 3 1) (injDiv 1 1)))
                                (injDiv 1 1)
                            )
                        )
            , test "Solve constant, 5" <|
                \() ->
                    Expect.equal
                        (solve
                            (Pred
                                (Less
                                    (ConstFact (Div 2 1) (injDiv 1 1))
                                    (ConstFact (Div 3 1) (injDiv 1 1))
                                )
                            )
                        )
                        (Pred
                            (Less (ConstFact (Div 1 3) (ConstFact (Div 2 1) (injDiv 1 1))) (injDiv 1 1))
                        )
            , test "Solve constant, 6" <|
                \() ->
                    Expect.equal (solve (Pred (Less (injDiv 1 1) (injDiv 1 1)))) (Pred (Less (injDiv 1 1) (injDiv 1 1)))
            , test "Left infinite projection 1" <|
                \() ->
                    Expect.equal
                        (leftInfProj (withDefault Bot (parseProp "(exists x (< x 1/1))")))
                        ( Top, [ injDiv 1 1 ] )
            , test "Left infinite projection 2" <|
                \() ->
                    Expect.equal
                        (leftInfProj
                            (withDefault
                                Bot
                                (parseProp "(exists x (/\\ (< x 1/2) (< x 1/1)))")
                            )
                        )
                        ( Conj Top Top, [ injDiv 1 2, injDiv 1 1 ] )
            , test "Left infinite projection 3" <|
                \() ->
                    Expect.equal
                        (leftInfProj
                            (withDefault
                                Bot
                                (parseProp "(exists x (\\/ (< x 1/2) (< x 1/1)))")
                            )
                        )
                        ( Disj Top Top, [ injDiv 1 2, injDiv 1 1 ] )
            , test "Right infinite projection 1" <|
                \() ->
                    Expect.equal
                        (rightInfProj (withDefault Bot (parseProp "(exists x (< x 1/1))")))
                        ( Bot, [ injDiv 1 1 ] )
            , test "Right infinite projection 2" <|
                \() ->
                    Expect.equal
                        (rightInfProj (withDefault Bot (parseProp "(exists x (< x 1/1))")))
                        ( Bot, [ injDiv 1 1 ] )
            , test "Right infinite projection 3" <|
                \() ->
                    Expect.equal
                        (rightInfProj (withDefault Bot (parseProp "(exists x (= x 10/1))")))
                        ( Bot, [ injDiv 10 1 ] )
            , test "Construct F3 1" <|
                \() ->
                    Expect.equal
                        (constructF3 (withDefault Bot (parseProp "(exists x (< x 10/1))")))
                        [ (withDefault Bot (parseProp "(< (* 1/2 (+ 10/1 10/1)) 10/1)")) ]
            , test "Construct F3 2" <|
                \() ->
                    Expect.equal
                        (List.length
                            (constructF3
                                (withDefault Bot
                                    (parseProp "(exists x (/\\ (< x 10/1) (< x 5/1)))")
                                )
                            )
                        )
                        4
            , describe "Solver case" solverTestCases
            , describe "Normalization case" normalizationTestCases
            ]
        ]


parserTest : Test
parserTest =
    describe "Parser test suite"
        [ describe "Parser test cases"
            [ test "Parser 1" <|
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
                        (Just
                            (Neg
                                (Forall (VI "x")
                                    (Pred
                                        (Less (Var 0 (VI "x"))
                                            (Var 0 (VI "x"))
                                        )
                                    )
                                )
                            )
                        )
            , test "Parser 5" <|
                \() ->
                    Expect.equal
                        (parseProp "(< (+ 1/1 1/1) (+ 0/1 1/1))")
                        (Just
                            (Pred
                                (Less
                                    (Plus (injDiv 1 1) (injDiv 1 1))
                                    (Plus (injDiv 0 1) (injDiv 1 1))
                                )
                            )
                        )
            , test "Parser 6" <|
                \() ->
                    Expect.equal
                        (parseProp "(forall x (< x x))")
                        (Just
                            (Forall (VI "x")
                                (Pred (Less (Var 0 (VI "x")) (Var 0 (VI "x"))))
                            )
                        )
            , test "Parser 7" <|
                \() ->
                    Expect.equal
                        (parseProp "(forall x (< x x))")
                        (Just
                            (Forall (VI "x")
                                (Pred
                                    (Less (Var 0 (VI "x")) (Var 0 (VI "x")))
                                )
                            )
                        )
            , test "Parser 8" <|
                \() ->
                    Expect.equal
                        (parseProp "(~ (forall x (< x x)")
                        Nothing
            , test "Parser 9" <|
                \() ->
                    Expect.equal
                        (parseProp "(forall x (< (* 3/1 x) (* 2/1 x)))")
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
                        (parseProp "(forall x (< (* 3/1 x) 1/1))")
                        (Just
                            (Forall (VI "x")
                                (Pred
                                    (Less
                                        (ConstFact (Div 3 1) (Var 0 (VI "x")))
                                        (injDiv 1 1)
                                    )
                                )
                            )
                        )
            ]
        ]


valOf : Maybe Prop -> Prop
valOf m =
    case m of
        Just x ->
            x

        Nothing ->
            Bot


solverTestCases : List Test
solverTestCases =
    [ describe "Solver test suite"
        [ describe "Parser test cases"
            [ test "Solver case 1" <|
                \() ->
                    Expect.equal
                        (solve (valOf (parseProp "(exists x (< (* 3/1 x) 5/1))")))
                        (valOf (parseProp "(exists x (< x (* 1/3 5/1)))"))
            , test "Solver case 2" <|
                \() ->
                    Expect.equal
                        (solve (valOf (parseProp "(exists x (< (+ x 1/1) 5/1))")))
                        (valOf (parseProp "(exists x (< x (- 5/1 1/1)))"))
            , test "Solver case 3: `x + 1 = 5 ==> x = 5-1`." <|
                \() ->
                    Expect.equal
                        (solve (valOf (parseProp "(exists x (= (+ x 1/1) 5/1))")))
                        (valOf (parseProp "(exists x (= x (- 5/1 1/1)))"))
            , test "Solver case 4: TODO: description." <|
                \() ->
                    Expect.equal
                        (solve (valOf (parseProp "(exists x (< (+ x (+ 1/1 2/1)) 5/1))")))
                        (valOf (parseProp "(exists x (< x (- 5/1 (+ 1/1 2/1))))"))
            , test "Solver case 5: TODO: description." <|
                \() ->
                    Expect.equal
                        (solve (valOf (parseProp "(exists x (< (+ (* 3/1 x) 1/1) 5/1))")))
                        (valOf (parseProp "(exists x (< x (* 1/3 (- 5/1 1/1))))"))
            , test "Solver case 6: TODO: description." <|
                \() ->
                    Expect.equal
                        (solve (valOf (parseProp "(exists x (= (+ (* 3/1 x) 1/1) 5/1))")))
                        (valOf (parseProp "(exists x (= x (* 1/3 (- 5/1 1/1))))"))
            , test "Solver case 7: TODO: description." <|
                \() ->
                    Expect.equal
                        (solve (valOf (parseProp "(exists x (< (* 3/1 x) (* 5/1 x)))")))
                        (valOf (parseProp "(exists x (< (- (* 3/1 x) (* 5/1 x)) 0/1))"))
            , test "Solver case 8: TODO: description." <|
                \() ->
                    Expect.equal
                        (solve (valOf (parseProp "(exists x (= (* 3/1 x) (* 5/1 x)))")))
                        (valOf (parseProp "(exists x (= (- (* 3/1 x) (* 5/1 x)) 0/1))"))
            ]
        ]
    ]

parse s = valOf (parseProp s)
    
normalizationTestCases : List Test
normalizationTestCases =
    [ describe "Normalization test suite"
        [ describe "Parser test cases"
            [ test "Normalization case 1: `normalizeExpr`." <|
                \() ->
                    Expect.equal
                        (normalizeExpr (Plus (ConstRat (Div 3 1))(ConstRat (Div 7 1))))
                        10.0
            , test "Normalization case 2: `normalizeRatPred`." <|
                \() ->
                    Expect.equal
                        (normalizeRatPred
                          (Less
                            (ConstRat (Div 1 1))
                            (Plus (injDiv 1 1) (injDiv 1 1))))
                        True
            , test "Normalization case 3: `normalizeRatPred`." <|
                \() ->
                    Expect.equal
                        (normalizeRatPred
                          (Less
                            (Plus (injDiv 1 1) (injDiv 1 1))
                            (ConstRat (Div 1 1))))
                        False
            , test "Normalization case 4: `normalize`." <|
                \() ->
                    Expect.equal
                        (normalize (parse "(/\\ true true)"))
                        True
            , test "Normalization case 5: `normalize`." <|
                \() ->
                    Expect.equal
                        (normalize (parse "(/\\ false true)"))
                        False
            , test "Normalization case 6: `normalize`." <|
                \() ->
                    Expect.equal
                        (normalize (parse "(\\/ false true)"))
                        True
            ]
        ]
    ]

satisfiabilityTestCases : List Test
satisfiabilityTestCases =
    [ describe "Satisfiability test suite"
        [ describe "Satisfiability test cases"
            [ test "Satisfiability case 1: simple." <|
                \() ->
                    Expect.equal
                      (isSat (parse "(exists x (< x (* 1/3 x)))"))
                      True
            , test "Satisfiability case 1: simple." <|
                \() ->
                    Expect.equal
                      (isSat (parse "(exists x (< x (* 1/3 x)))"))
                      True
            ]
        ]
    ]
