module OmitNegations exposing (removeAllNegations)

import Syntax exposing (Prop(..), RatPred(..))


-- Take a proposition `p` that is assumed to be in NNF
-- and remove all negations that are on literals.


removeAllNegations : Prop -> Prop
removeAllNegations p =
    case p of
        Neg (Pred (Less e1 e2)) ->
            Disj (Pred (Less e2 e1)) (Pred (Eq e1 e2))

        -- this case should not happen.
        Neg (Pred (Eq e1 e2)) ->
            Disj (Pred (Less e1 e2)) (Pred (Less e1 e2))

        Conj p1 p2 ->
            Conj (removeAllNegations p1) (removeAllNegations p2)

        Disj p1 p2 ->
            Disj (removeAllNegations p1) (removeAllNegations p2)

        Forall s p1 ->
            Forall s (removeAllNegations p1)

        Exists s p1 ->
            Exists s (removeAllNegations p1)

        Neg Top ->
            Bot

        Neg Bot ->
            Top

        Neg p1  -> Neg (removeAllNegations p1)

        p -> p
