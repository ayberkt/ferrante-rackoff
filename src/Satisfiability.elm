module Satisfiability exposing (isSat, decideFinal)

import Normalization       exposing (normalize)
import InfiniteProjection  exposing (leftInfProj, rightInfProj, constructF3)
import Syntax              exposing (..)

decideFinal : Prop -> Prop -> List Prop -> Bool
decideFinal l r ps =
  if (normalize l) || (normalize r) then
    True
  else
    List.foldl (||) False (List.map (normalize) ps)

type SimplifiedProp = Prop

type Result =
    NoExistentialFound
  | Existential Prop
  | NegatedExistential Prop

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

isSat : Prop -> Bool
isSat sp =
    case getInnermostExistential sp NoExistentialFound of
      Existential sp ->
        let
          (leftProj,  _)  = leftInfProj sp
          (rightProj, _)  = rightInfProj sp
          middleCases     = constructF3 sp
        in
          decideFinal leftProj rightProj middleCases

      NegatedExistential sp ->
        let
          (leftProj,  _)  = leftInfProj sp
          (rightProj, _)  = rightInfProj sp
          middleCases     = constructF3 sp
        in
          not (decideFinal leftProj rightProj middleCases)

      NoExistentialFound -> False
