module Normalization exposing (normalizeExpr, normalizeRatPred, normalize)

import PropositionParser exposing (subst)
import Syntax            exposing (Prop(..), RatPred(..), Expr(..), Rat(..))
import Maybe             exposing (andThen)
import Set

-- Evaluate an arithmetic expression to its numerical value.
normalizeExpr : Expr -> Maybe Float
normalizeExpr e =
  case e of
    Plus  e1 e2 ->
      normalizeExpr e1 |>
        andThen (\e1n ->
          normalizeExpr e2 |>
            andThen (\e2n ->
              Just (e1n - e2n)))
    Minus e1 e2 ->
      normalizeExpr e1 |>
        andThen (\e1n ->
          normalizeExpr e2 |>
            andThen (\e2n ->
              Just (e1n - e2n)))
    ConstRat  (Div r1 r2)    -> Just (toFloat r1 / toFloat r2)
    ConstFact (Div r1 r2) e1 ->
      normalizeExpr e1 |>
        andThen (\e1n -> Just ((toFloat r1 / toFloat r2) * e1n))
    Var _ _ -> Nothing

-- Evaluate a rational predicate to its Boolean value.
normalizeRatPred : RatPred -> Maybe Bool
normalizeRatPred rp =
  case rp of
    Less    e1 e2 ->
      normalizeExpr e1
        |> andThen (\e1n ->
      normalizeExpr e2
        |> andThen (\e2n -> Just (e1n < e2n)))
    Greater e1 e2 ->
      normalizeExpr e1
        |> andThen (\e1n ->
      normalizeExpr e2
        |> andThen (\e2n -> Just (e1n > e2n)))
    Eq e1 e2 ->
      normalizeExpr e1
        |> andThen (\e1n ->
      normalizeExpr e2
        |> andThen (\e2n -> Just (e1n == e2n)))

-- Evaluate a proposition to its Boolean value.
normalize : Prop -> Maybe Bool
normalize p =
  case p of
    Neg p1      ->
      normalize p1 |> (andThen (\p1n -> Just (not p1n)))
    Conj p1 p2  ->
      normalize p1
        |> (andThen (\p1n ->
      normalize p2
        |> (andThen (\p2n -> Just (p1n && p2n)))))
    Disj p1 p2  ->
      normalize p1
        |> (andThen (\p1n ->
      normalize p2
        |> (andThen (\p2n -> Just (p1n || p2n)))))
    Top         -> Just True
    Bot         -> Just False
    Pred rp     -> normalizeRatPred rp
    Id s        -> Just False
    Exists s e1 -> Just False
    Forall s e1 -> Just False
