module Syntax exposing (Prop(..), RatPred(..), linearize)

type Expr =
    Zero
  | One
  | Plus
  | Minus
  | Var Int String

type RatPred =
    Eq      Expr Expr
  | Greater Expr Expr
  | Less    Expr Expr

type Prop =
    Pred RatPred
  | Top
  | Bot
  | Neg Prop
  | Conj Prop Prop
  | Disj Prop Prop
  | Forall Prop
  | Exists Prop

linearizeRatPred : RatPred -> String
linearizeRatPred rp = "TODO"

linearize : Prop -> String
linearize p =
  case p of
    Top           -> "\\top"
    Bot           -> "\\bot"
    Conj    p1 p2 -> (linearize p1) ++ "\\wedge" ++ (linearize p2)
    Disj    p1 p2 -> (linearize p1) ++ "\\vee"   ++ (linearize p2)
    Forall  p1    -> "\\forall.\\ " ++ (linearize p1)
    Exists  p1    -> "\\exists.\\ " ++ (linearize p1)
    _             -> "TODO"
