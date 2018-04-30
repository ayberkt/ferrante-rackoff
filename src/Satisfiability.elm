module Satisfiability exposing (isSat, decideFinal, getInnermostExistential, replace, Result(..))

import Normalization       exposing (normalize)
import InfiniteProjection  exposing (leftInfProj, rightInfProj, constructF3)
import Syntax              exposing (..)

decideFinal : Prop -> Prop -> List Prop -> DecisionResult
decideFinal l r ps =
  case (normalize l, normalize r) of
    (Just  True,  _) -> Conclusion True
    (_, Just True) -> Conclusion True
    (Just False,  Just  False) -> Conclusion False -- TODO: implement this case.
    (Nothing, Nothing) -> Conclusion False
    (Nothing, Just False) -> Conclusion False
    (Just False, Nothing) -> Conclusion False

type SimplifiedProp = Prop

type Result =
    NoExistentialFound
  | Existential Prop
  | NegatedExistential Prop

-- When we attempt to decide a formula, we have two possible outcomes: we
-- either find out that the existential evaluate to a Boolean value or we
-- get a quantifier-free formula which will now be a subformula of the outer
-- existential quantification.
type DecisionResult =
    Conclusion Bool
  | QuantifierFree Prop

getInnermostExistential : Prop -> Result -> Result
getInnermostExistential sp lastSeen =
  case sp of
    Exists s sp1       ->
      getInnermostExistential sp1 (Existential (Exists s sp1))
    Neg (Exists s sp1) ->
      getInnermostExistential sp1 (NegatedExistential (Exists s sp1))
    Pred _             -> lastSeen
    Bot                -> lastSeen
    Top                -> lastSeen
    Neg  sp1           -> getInnermostExistential sp1 lastSeen
    Conj sp1 sp2       ->
      case getInnermostExistential sp1 lastSeen of
        NoExistentialFound -> getInnermostExistential sp2 lastSeen
        other -> other
    Disj sp1 sp2 ->
      case getInnermostExistential sp1 lastSeen of
        NoExistentialFound -> getInnermostExistential sp2 lastSeen
        other -> other

    -- The following two cases must not happen.
    Id _ -> NoExistentialFound
    Forall _ _ -> NoExistentialFound

-- We take "simple" to mean a proposition either having one existential
-- quantification or none at all.
decideSimple sp =
  let
    (leftProj,  _)  = leftInfProj sp
    (rightProj, _)  = rightInfProj sp
    middleCases     = constructF3 sp
  in
    decideFinal leftProj rightProj middleCases

decideInnermostExistential : Prop -> (DecisionResult, Maybe Prop)
decideInnermostExistential sp =
    case getInnermostExistential sp NoExistentialFound of
      Existential        sp ->
        case decideSimple sp of
          Conclusion p      -> (Conclusion p, Just sp)
          QuantifierFree p  -> (QuantifierFree p, Just sp)
      NegatedExistential sp ->
        case decideSimple sp of
          Conclusion p      -> (Conclusion (not p), Just sp)
          QuantifierFree p  -> (QuantifierFree p,   Just sp)
      NoExistentialFound    -> (decideSimple sp, Nothing)

-- TODO: implement the replace function so that existential quantifiers are
-- popped when going backwards after handling the innermost existential.
replace : Prop -> Prop -> Prop -> Prop
replace p exProp new =
  case p of
    Neg p1 ->
      if p1 == exProp then
        Neg new
      else
        Neg (replace p exProp new)
    Conj p1 p2 ->
      case (p1 == exProp, p2 == exProp) of
        (True,   True) -> Conj new new
        (True,  False) -> Conj new p2
        (False,  True) -> Conj p1  new
        (False, False) -> Conj p1  p2
    Disj p1 p2 ->
      case (p1 == exProp, p2 == exProp) of
        (True,   True) -> Conj new new
        (True,  False) -> Conj new p2
        (False,  True) -> Conj p1  new
        (False, False) -> Conj p1  p2
    Exists s p1 ->
      if p1 == exProp then
        Neg new
      else
        Neg (replace p exProp new)
    Forall _ _ -> Bot -- this case should not happen.
    other -> other

isSat : Prop -> DecisionResult
isSat sp =
  let
    (result, maybeInnermost) = decideInnermostExistential sp
  in
      case maybeInnermost of
        Just innermost -> result
        Nothing        -> result
