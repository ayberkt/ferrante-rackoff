module Normalization exposing (normalizeExpr, normalizeRatPred, normalize)

import Syntax exposing (Prop(..), RatPred(..), Expr(..), Rat(..))
import Set
import PropositionParser exposing (subst)

-- Evaluate an arithmetic expression to its numerical value.
normalizeExpr : Expr -> Float
normalizeExpr e =
  case e of
    Plus  e1 e2              -> normalizeExpr e1 + normalizeExpr e2
    Minus e1 e2              -> normalizeExpr e1 - normalizeExpr e2
    ConstRat  (Div r1 r2)    -> toFloat r1 / toFloat r2
    ConstFact (Div r1 r2) e1 -> (toFloat r1 / toFloat r2) * normalizeExpr e1
    Var _ _ -> -1

-- Evaluate a rational predicate to its Boolean value.
normalizeRatPred : RatPred -> Bool
normalizeRatPred rp =
  case rp of
    Less    e1 e2 -> normalizeExpr e1 <  normalizeExpr e2
    Greater e1 e2 -> normalizeExpr e1 >  normalizeExpr e2
    Eq      e1 e2 -> normalizeExpr e1 == normalizeExpr e2

-- Evaluate a proposition to its Boolean value.
normalize : Prop -> Bool
normalize p =
  case p of
    Top         -> True
    Bot         -> False
    Neg p1      -> not (normalize p1)
    Conj p1 p2  -> normalize p1 && normalize p2
    Disj p1 p2  -> normalize p1 || normalize p2
    Pred rp     -> normalizeRatPred rp
    Id s        -> False
    Exists s e1 -> False
    Forall s e1 -> False
