module Syntax exposing (Prop(..), RatPred(..), Expr(..), linearize, linearizeExpr, VarIdentifier(..), Rat(..), occurs)

-- Newtype of `String` as a variable identifier.


type VarIdentifier
    = VI String



-- Arithmetic expressions.


type Rat
    = Div Int Int


type Expr
    = Plus Expr Expr
    | Minus Expr Expr
    | Var Int VarIdentifier
    | ConstRat Rat
    | ConstFact Rat Expr



-- Comparisons.


type RatPred
    = Eq Expr Expr
    | Less Expr Expr



-- Type of propositions.


type Prop
    = Pred RatPred
    | Id String
    | Top
    | Bot
    | Neg Prop
    | Conj Prop Prop
    | Disj Prop Prop
    | Forall VarIdentifier Prop
    | Exists VarIdentifier Prop



-- Project the `String` from a given `VarIdentifier`


show : VarIdentifier -> String
show (VI s) =
    s



-- Linearize a given `Expr` `e`.


linearizeRat : Rat -> String
linearizeRat q =
    case q of
        Div n 1 ->
            toString n

        Div 0 n ->
            "0"

        Div _ 0 ->
            "ERROR: DIVISION BY ZERO"

        Div n1 n2 ->
            "(" ++ toString n1 ++ "/" ++ toString n2 ++ ")"


linearizeExpr : Expr -> String
linearizeExpr e =
    case e of
        Plus e1 e2 ->
            "(" ++ linearizeExpr e1 ++ "+" ++ linearizeExpr e2 ++ ")"

        Minus e1 e2 ->
            "(" ++ linearizeExpr e1 ++ "-" ++ linearizeExpr e2 ++ ")"

        ConstFact c e ->
            (linearizeRat c) ++ "(" ++ linearizeExpr e ++ ")"

        ConstRat (Div z1 z2) ->
            linearizeRat (Div z1 z2)

        Var n x ->
            "x" ++ toString n



-- Linearize a given `RatPred` `rp`.


linearizeRatPred : RatPred -> String
linearizeRatPred rp =
    case rp of
        Eq e1 e2 ->
            "(" ++ linearizeExpr e1 ++ "  =  " ++ linearizeExpr e2 ++ ")"

        Less e1 e2 ->
            "(" ++ linearizeExpr e1 ++ "  <  " ++ linearizeExpr e2 ++ ")"



-- Linearize a given `Prop` `p`.


occurs : Int -> Expr -> Bool
occurs i e =
    case e of
        Plus e1 e2 ->
            (occurs i e2) || (occurs i e2)

        Minus e1 e2 ->
            (occurs i e1) || (occurs i e2)

        Var i_ vi ->
            i == i_

        ConstRat r ->
            False

        ConstFact r e1 ->
            occurs i e1


linearize : Prop -> String
linearize p =
    case p of
        Top ->
            "⊤"

        Bot ->
            "⊥"

        Conj p1 p2 ->
            "(" ++ (linearize p1) ++ "∧" ++ (linearize p2) ++ ")"

        Disj p1 p2 ->
            "(" ++ (linearize p1) ++ "∨" ++ (linearize p2) ++ ")"

        Forall s p1 ->
            "(" ++ "∀" ++ show s ++ ". " ++ (linearize p1) ++ ")"

        Exists s p1 ->
            "(" ++ "∃" ++ show s ++ ". " ++ linearize p1 ++ ")"

        Neg p1 ->
            "¬" ++ linearize p1

        Pred rp ->
            linearizeRatPred rp

        Id x ->
            x
