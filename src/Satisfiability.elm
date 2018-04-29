module Satisfiability exposing (isSat)

import Normalization exposing (normalize)
import Syntax        exposing (..)

isSat : Prop -> Prop -> List Prop -> Bool
isSat l r ps =
  if (normalize l) || (normalize r) then
    True
  else
    List.foldl (||) False (List.map (normalize) ps)
