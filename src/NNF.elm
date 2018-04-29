module NNF exposing (convertToNNF)

import Syntax exposing (Prop(..), VarIdentifier(..), linearize)


-- Take a `Prop` `p` and push all negations it contains inside by
--   * converting negated universals to existentials (and vice versa),
--   * pushing negations into binary operators via de Morgan laws.


convertToNNF : Prop -> Prop
convertToNNF p =
    case p of
        Neg (Conj p1 p2) ->
            Disj (convertToNNF (Neg p1)) (convertToNNF (Neg p2))

        Neg (Disj p1 p2) ->
            Conj (convertToNNF (Neg p1)) (convertToNNF (Neg p2))

        Neg (Neg p) ->
            convertToNNF p

        Neg (Forall s p) ->
            Exists s (convertToNNF (Neg p))

        Neg (Exists s p) -> Neg (Exists s (convertToNNF p))

        Conj p1 p2 ->
            Conj (convertToNNF p1) (convertToNNF p2)

        Disj p1 p2 ->
            Disj (convertToNNF p1) (convertToNNF p2)

        Forall s p ->
            Neg (Exists s (convertToNNF (Neg p)))

        Exists s p ->
            Exists s (convertToNNF p)

        p ->
            p
