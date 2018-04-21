module InfiniteProjection exposing (..)

import Syntax exposing (Prop(..), RatPred(..), Expr(..))


leftInfProj : Prop -> Prop
leftInfProj p =
    case p of
        Pred (Less (Var _ _) a) ->
            Top

        Pred (Less a (Var _ x)) ->
            Bot

        Pred (Eq (Var _ x) c) ->
            Bot

        Pred rp ->
            Pred rp

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
