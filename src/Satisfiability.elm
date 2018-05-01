module Satisfiability
       exposing (
         isSat
       , decideFinal
       , getExistentials
       , replace
       , Result(..)
       , DecisionResult(..)
       )

import Normalization       exposing (normalize)
import InfiniteProjection  exposing (leftInfProj, rightInfProj, constructF3)
import Syntax              exposing (..)

holds p =
  case p of
    Just True  -> True
    _          -> False

decideFinal : Prop -> Prop -> List Prop -> DecisionResult
decideFinal l r ps =
  case (normalize l, normalize r) of
    (Just  True,  _) -> Conclusion True
    (_, Just True) -> Conclusion True
    (Just False,  Just  False) -> Conclusion (List.any holds (List.map normalize ps))
    (Nothing, Nothing)    -> Conclusion (List.any holds (List.map normalize ps))
    (Nothing, Just False) -> Conclusion (List.any holds (List.map normalize ps))
    (Just False, Nothing) ->  Conclusion (List.any holds (List.map normalize ps))

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

getExistentials : Prop -> List Result
getExistentials sp =
  case sp of
    Exists s sp1       -> (Existential (Exists s sp1)) :: (getExistentials sp1)
    Neg (Exists s sp1) -> (NegatedExistential (Exists s sp1)) :: (getExistentials sp1)
    Pred _             -> []
    Bot                -> []
    Top                -> []
    Neg  sp1           -> getExistentials sp1
    Conj sp1 sp2       -> (getExistentials sp1) ++ (getExistentials sp2)
    Disj sp1 sp2 -> (getExistentials sp1) ++ (getExistentials sp2)
    -- The following two cases must not happen.
    Id _ -> []
    Forall _ _ -> []

-- We take "simple" to mean a proposition either having one existential
-- quantification or none at all.
decideSimple sp =
  let
    (leftProj,  _)  = leftInfProj sp
    (rightProj, _)  = rightInfProj sp
    middleCases     = constructF3 sp
  in
    if List.any holds (List.map normalize middleCases) then
      Conclusion True
    else
      decideFinal leftProj rightProj middleCases

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
isSat sp = decideSimple sp
