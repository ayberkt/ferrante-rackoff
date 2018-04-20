module InfiniteProjection exposing (..)

import Syntax exposing (Prop(..))


leftInfProj : Prop -> Prop
leftInfProj p =
    case p of
        Pred rp ->
            Pred rp

        -- TODO
        Id s ->
            Id s

        Top ->
            Top

        Bot ->
            Bot

        Neg p ->
            Neg (leftInfProj p)

        Conj p1 p2 ->
            Conj (leftInfProj p1) (leftInfProj p2)

        Disj p1 p2 ->
            Disj (leftInfProj p1) (leftInfProj p2)

        Forall vi p ->
            Forall vi (leftInfProj p)

        Exists vi p ->
            Exists vi (leftInfProj p)



-- TODO


rightInfProj : Prop -> Prop
rightInfProj p =
    p



-- TODO
