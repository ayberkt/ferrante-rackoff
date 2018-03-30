module Syntax exposing (Prop(..), linearize)

type Expr =
    Zero
  | One
  | Plus
  | Minus
  | Var Int String

type RatPred =
    Eq      Expr Expr
  | Greater Expr Expr

type Prop =
    RatPred
  | Top
  | Bot
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
