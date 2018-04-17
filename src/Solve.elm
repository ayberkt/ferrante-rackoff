module Solve exposing (solve)

import Syntax exposing (Prop(..), RatPred(..), Expr(..), Rat(..))


-- Take a proposition `p` that is assumed to be in NNF
-- and remove all negations that are on literals.
-- Negate a given rational number i.e., divide 1 by the given number.


negateByMul : Rat -> Rat
negateByMul (Div n1 n2) =
    Div n2 n1


solveRatPred : RatPred -> RatPred
solveRatPred p =
    case p of
        Less t (ConstFact c x) ->
            Less (ConstFact (negateByMul c) t) x

        p_ ->
            p_


solve : Prop -> Prop
solve p =
    case p of
        Top ->
            Top

        Bot ->
            Bot

        Conj p1 p2 ->
            Conj (solve p1) (solve p2)

        Disj p1 p2 ->
            Disj (solve p1) (solve p2)

        Forall s p1 ->
            Forall s (solve p1)

        Exists p1 ->
            Exists (solve p1)

        Neg p1 ->
            Neg (solve p1)

        -- this case should not happen.
        Pred rp ->
            Pred (solveRatPred rp)

        Id x ->
            Id x
