module Solve exposing (solve)

import Syntax exposing (Prop(..), RatPred(..), Expr(..), Rat(..), occurs)


-- Take a proposition `p` that is assumed to be in NNF
-- and remove all negations that are on literals.
-- Negate a given rational number i.e., divide 1 by the given number.


negateByMul : Rat -> Rat
negateByMul (Div n1 n2) =
    Div n2 n1



-- Decide if a predicate has been adequately solved.


solved : RatPred -> Bool
solved rp =
    case rp of
        Less e1 (Var i _) ->
            not (occurs i e1)

        Less (Var i _) e1 ->
            not (occurs i e1)

        Eq e1 (Var i _) ->
            not (occurs i e1)

        Eq (Var i _) e1 ->
            not (occurs i e1)

        Less e1 e2 ->
            (not (occurs 0 e1)) && (not (occurs 0 e2))

        Eq e1 e2 ->
            (not (occurs 0 e1)) && (not (occurs 0 e2))


isConstant : Expr -> Bool
isConstant e =
    case e of
        ConstRat _ ->
            True

        Var _ _ ->
            False

        ConstFact _ e2 ->
            isConstant e2

        Plus e1 e2 ->
            isConstant e1 && isConstant e2

        Minus e1 e2 ->
            isConstant e1 && isConstant e2


removeConstantAdditions : RatPred -> RatPred
removeConstantAdditions e =
    case e of
        Less (Plus e1 e2) e3 ->
            case ( isConstant e1, isConstant e2 ) of
                ( False, True ) ->
                    Less e1 (Minus e3 e2)

                ( True, False ) ->
                    Less e1 (Minus e3 e1)

                ( _, _ ) ->
                    e

        Eq (Plus e1 e2) e3 ->
            case ( isConstant e1, isConstant e2 ) of
                ( False, True ) ->
                    Eq e1 (Minus e3 e2)

                ( True, False ) ->
                    Eq e1 (Minus e3 e1)

                ( _, _ ) ->
                    e

        e1 ->
            e


moveVarToLHS : RatPred -> RatPred
moveVarToLHS rp =
    case rp of
        Less e1 e2 ->
            if occurs 0 e2 then
                Less (Minus e1 e2) (ConstRat (Div 0 1))
            else
                Less e1 e2

        Eq e1 e2 ->
            if occurs 0 e2 then
                Eq (Minus e1 e2) (ConstRat (Div 0 1))
            else
                Eq e1 e2



-- Replace every predicate of the form t < cx with t/c < x so that
-- variables are alone. This corresponds to Step 4 of the algorithm


normMuls : Expr -> Expr
normMuls e =
    case e of
        Plus e1 e2 ->
            Plus (normMuls e1) (normMuls e2)

        Minus e1 e2 ->
            Minus (normMuls e1) (normMuls e2)

        Var i vi ->
            Var i vi

        ConstRat r ->
            ConstRat r

        ConstFact (Div z1_1 z1_2) (ConstFact (Div z2_1 z2_2) e1) ->
            ConstFact (Div (z1_1 * z2_1) (z1_2 * z2_2)) (normMuls e1)

        ConstFact (Div z1_1 z1_2) (ConstRat (Div z2_1 z2_2)) ->
            ConstRat (Div (z1_1 * z2_1) (z1_2 * z2_2))

        ConstFact r e1 ->
            ConstFact r (normMuls e1)


omitCoefficients : RatPred -> RatPred
omitCoefficients rp =
    case rp of
        Less t (ConstFact c e1) ->
            Less (ConstFact (negateByMul c) t) e1

        Less (ConstFact c e1) t ->
            Less e1 (ConstFact (negateByMul c) t)

        Eq t (ConstFact c x) ->
            Eq (ConstFact (negateByMul c) t) x

        Eq (ConstFact c x) t ->
            Eq x (ConstFact (negateByMul c) t)

        Less e1 e2 ->
            Less e1 e2

        Eq e1 e2 ->
            Eq e1 e2


solveRatPred : RatPred -> RatPred
solveRatPred =
    -- \e -> moveVarToLHS (omitCoefficients (removeConstantAdditions e))
    \e -> omitCoefficients (removeConstantAdditions (moveVarToLHS e))



-- Go down recursively and apply `solveRatPred` to predicates.


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

        Exists s p1 ->
            Exists s (solve p1)

        Neg p1 ->
            Neg (solve p1)

        -- this case should not happen.
        Pred rp ->
            Pred (solveRatPred rp)

        Id x ->
            Id x
