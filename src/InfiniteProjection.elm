module InfiniteProjection exposing (leftInfProj, rightInfProj, constructF3)

import Syntax exposing (Prop(..), RatPred(..), Expr(..), Rat(..))
import Set
import PropositionParser exposing (subst)


-- Taken from circuithub/elm-list-extra.


nub : List Expr -> List Expr -> List Expr
nub xs hist =
    case xs of
        [] ->
            hist

        x :: xs ->
            (if List.member x hist then
                nub xs hist
             else
                nub xs (x :: hist)
            )


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
                ( p1LInfProj, replaced )


leftInfProj =
    \x -> infProj x LeftInfProj


rightInfProj =
    \x -> infProj x RightInfProj


listProduct : List a -> List a -> List ( a, a )
listProduct xs ys =
    List.concat (List.map (\x -> List.map (\y -> ( x, y )) ys) xs)


bigVee : List Expr -> (( Expr, Expr ) -> Expr) -> Prop -> List Prop
bigVee ps f pfree =
    List.map (\( x, y ) -> subst pfree 0 (f ( x, y ))) (listProduct ps ps)


constructF3 : Prop -> List Prop
constructF3 p =
    case p of
        Exists vi p1 ->
            let
                ( _, replaced1 ) =
                    leftInfProj (Exists vi p1)

                ( _, replaced2 ) =
                    rightInfProj (Exists vi p1)
            in
                bigVee
                    (nub (replaced1 ++ replaced2) [])
                    (\( x, y ) -> (ConstFact (Div 1 2) (Plus x y)))
                    p1

        p_ ->
            [ p_ ]
