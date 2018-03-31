module NNF exposing (convertToNNF)

import Syntax exposing (Prop(..), linearize)


convertToNNF : Prop -> Prop
convertToNNF p =
    case p of
        Neg (Conj p1 p2) ->
            Disj (convertToNNF (Neg p1)) (convertToNNF (Neg p2))

        Neg (Disj p1 p2) ->
            Conj (convertToNNF (Neg p1)) (convertToNNF (Neg p2))

        Neg (Neg p) ->
            convertToNNF p

        Neg (Forall _ p) ->
            Exists (convertToNNF (Neg p))

        Neg (Exists p) ->
            Forall "TODO" (convertToNNF (Neg p))

        Conj p1 p2 ->
            Conj (convertToNNF p1) (convertToNNF p2)

        Disj p1 p2 ->
            Disj (convertToNNF p1) (convertToNNF p2)

        Forall s p ->
            Forall s (convertToNNF p)

        Exists p ->
            Exists (convertToNNF p)

        p ->
            p
