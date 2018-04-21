module InfiniteProjection exposing (leftInfProj, rightInfProj)

import Syntax exposing (Prop(..), RatPred(..), Expr(..))
import PropositionParser exposing (subst)


mkEmptyReplacedList x =
    ( x, [] )


type InfiniteProjectionKind
    = LeftInfProj
    | RightInfProj


infProj : Prop -> InfiniteProjectionKind -> ( Prop, List Expr )
infProj p ipk =
    case p of
        Pred (Less (Var _ _) a) ->
            case ipk of
                LeftInfProj ->
                    ( Top, [ a ] )

                RightInfProj ->
                    ( Bot, [ a ] )

        Pred (Less a (Var _ x)) ->
            case ipk of
                RightInfProj ->
                    ( Top, [ a ] )

                LeftInfProj ->
                    ( Bot, [ a ] )

        Pred (Eq (Var _ x) c) ->
            case ipk of
                RightInfProj ->
                    ( Bot, [ c ] )

                LeftInfProj ->
                    ( Bot, [ c ] )

        Pred rp ->
            mkEmptyReplacedList (Pred rp)

        Id s ->
            mkEmptyReplacedList (Id s)

        Top ->
            mkEmptyReplacedList Top

        Bot ->
            mkEmptyReplacedList (Bot)

        Neg p1 ->
            let
                ( p1LInfProj, replaced ) =
                    infProj p1 ipk
            in
                ( Neg p1LInfProj, replaced )

        Conj p1 p2 ->
            let
                ( p1LInfProj, replaced1 ) =
                    infProj p1 ipk

                ( p2LInfProj, replaced2 ) =
                    infProj p2 ipk
            in
                ( Conj p1LInfProj p2LInfProj, replaced1 ++ replaced2 )

        Disj p1 p2 ->
            let
                ( p1LInfProj, replaced1 ) =
                    infProj p1 ipk

                ( p2LInfProj, replaced2 ) =
                    infProj p2 ipk
            in
                ( Disj p1LInfProj p2LInfProj, replaced1 ++ replaced2 )

        Forall vi p ->
            let
                ( p1LInfProj, replaced ) =
                    infProj p ipk
            in
                ( Forall vi p1LInfProj, replaced )

        Exists vi p ->
            let
                ( p1LInfProj, replaced ) =
                    infProj p ipk
            in
                ( Exists vi p1LInfProj, replaced )


leftInfProj =
    \x -> infProj x LeftInfProj


rightInfProj =
    \x -> infProj x RightInfProj



-- constructF3 : Prop -> Prop
-- constructF3 p =
-- case p of
-- (Exists _ p1) -> (su
-- p1 -> p1
-- TODO
