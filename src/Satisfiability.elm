module Satisfiability exposing (isSat)

import Normalization exposing (normalize)
import Syntax        exposing (..)

isSat : Prop -> Prop -> List Prop -> Bool
isSat l r ps =
  if (normalize l) || (normalize r) then
    True
  else
    List.foldl (||) False (List.map (normalize) ps)

type SimplifiedProp = Prop

getInnermostExistential : SimplifiedProp -> SimplifiedProp
getInnermostExistential sp r =
  case sp of
    Neg  sp1     ->
    Conj sp1 sp2 ->
    Disj sp1 sp2 ->
    E

isSat : SimplifiedProp -> Bool
isSat p = False -- TODO
