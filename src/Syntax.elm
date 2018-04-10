module Syntax exposing (Prop(..), RatPred(..), Expr(..), linearize, VarIdentifier(..))

-- Newtype of `String` as a variable identifier.


type VarIdentifier
    = VI String



-- Arithmetic expressions.


type Expr
    = Zero
    | One
    | Plus Expr Expr
    | Minus Expr Expr
    | Var Int VarIdentifier



-- Comparisons.


type RatPred
    = Eq Expr Expr
    | Greater Expr Expr
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
    | Exists Prop



-- Project the `String` from a given `VarIdentifier`


show : VarIdentifier -> String
show (VI s) =
    s



-- Linearize a given `Expr` `e`.


linearizeExpr : Expr -> String
linearizeExpr e =
    case e of
        Zero ->
            "0"

        One ->
            "1"

        Plus e1 e2 ->
            "(" ++ linearizeExpr e1 ++ "+" ++ linearizeExpr e2 ++ ")"

        Minus e1 e2 ->
            "(" ++ linearizeExpr e1 ++ "-" ++ linearizeExpr e2 ++ ")"

        Var n x ->
            "x" ++ toString n



-- Linearize a given `RatPred` `rp`.


linearizeRatPred : RatPred -> String
linearizeRatPred rp =
    case rp of
        Eq e1 e2 ->
            "(" ++ linearizeExpr e1 ++ "=" ++ linearizeExpr e2 ++ ")"

        Greater e1 e2 ->
            "(" ++ linearizeExpr e1 ++ ">" ++ linearizeExpr e2 ++ ")"

        Less e1 e2 ->
            "(" ++ linearizeExpr e1 ++ "<" ++ linearizeExpr e2 ++ ")"



-- Linearize a given `Prop` `p`.


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

        Exists p1 ->
            "(" ++ "∃" ++ linearize p1 ++ ")"

        Neg p1 ->
            "¬" ++ linearize p1

        Pred rp ->
            linearizeRatPred rp

        Id x ->
            x
