module Syntax exposing (Prop(..), RatPred(..), Expr(..), linearize)


type Expr
    = Zero
    | One
    | Plus Expr Expr
    | Minus Expr Expr
    | Var Int String


type RatPred
    = Eq Expr Expr
    | Greater Expr Expr
    | Less Expr Expr


type Prop
    = Pred RatPred
    | Id String
    | Top
    | Bot
    | Neg Prop
    | Conj Prop Prop
    | Disj Prop Prop
    | Forall String Prop
    | Exists Prop


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
            "x@" ++ toString n


linearizeRatPred : RatPred -> String
linearizeRatPred rp =
    case rp of
        Eq e1 e2 ->
            "(" ++ linearizeExpr e1 ++ "=" ++ linearizeExpr e2 ++ ")"

        Greater e1 e2 ->
            "(" ++ linearizeExpr e1 ++ ">" ++ linearizeExpr e2 ++ ")"

        Less e1 e2 ->
            "(" ++ linearizeExpr e1 ++ "<" ++ linearizeExpr e2 ++ ")"


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
            "(" ++ "∀" ++ s ++ ". " ++ (linearize p1) ++ ")"

        Exists p1 ->
            "\\exists.\\ " ++ (linearize p1)

        Neg p1 ->
            "¬" ++ linearize p1

        Pred rp ->
            linearizeRatPred rp

        Id x ->
            x
