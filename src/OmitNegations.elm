module OmitNegations exposing (removeAllNegations)

import Syntax exposing (Prop(..), RatPred(..))


removeAllNegations : Prop -> Prop
removeAllNegations p =
    case p of
        Neg (Pred (Less e1 e2)) ->
            Disj (Pred (Less e2 e1)) (Pred (Eq e1 e2))

        Neg (Pred (Eq e1 e2)) ->
            Disj (Pred (Less e1 e2)) (Pred (Greater e1 e2))

        -- this case should not happen.
        Conj p1 p2 ->
            Conj (removeAllNegations p1) (removeAllNegations p2)

        Disj p1 p2 ->
            Disj (removeAllNegations p1) (removeAllNegations p2)

        Forall s p1 ->
            Forall s (removeAllNegations p1)

        Exists p1 ->
            Exists (removeAllNegations p1)

        Neg Top ->
            Bot

        Neg Bot ->
            Top

        p ->
            p
