module PropositionParser exposing (parseProp, subst)

import Parser
    exposing
        ( Parser
        , (|.)
        , (|=)
        , succeed
        , symbol
        , float
        , ignore
        , zeroOrMore
        , lazy
        , oneOf
        , andThen
        , delayedCommit
        , run
        )
import Parser.LanguageKit exposing (variable)
import Char
import Set
import Util
import Maybe
import Syntax exposing (Prop(..), RatPred(..), Expr(..), VarIdentifier(..), Rat(..))


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')


infixOp r =
    symbol "("
        |> andThen
            (\_ ->
                oneOf
                    [ delayedCommit (symbol "/\\")
                        (succeed
                            Conj
                            |. spaces
                            |= lazy r
                            |. spaces
                            |= lazy r
                            |. symbol ")"
                        )
                    , delayedCommit (symbol "\\/")
                        (succeed
                            Disj
                            |. spaces
                            |= lazy r
                            |. spaces
                            |= lazy r
                            |. symbol ")"
                        )
                    , delayedCommit (symbol "~")
                        (succeed
                            Neg
                            |. spaces
                            |= lazy r
                            |. spaces
                            |. symbol ")"
                        )
                    , delayedCommit (symbol "forall")
                        (succeed
                            Forall
                            |. spaces
                            |= (succeed VI |= variable Char.isLower isVarChar keywords)
                            |. spaces
                            |= lazy r
                            |. spaces
                            |. symbol ")"
                        )
                    , delayedCommit (symbol "exists")
                        (succeed
                            Exists
                            |. spaces
                            |= (succeed VI |= variable Char.isLower isVarChar keywords)
                            |. spaces
                            |= lazy r
                            |. spaces
                            |. symbol ")"
                        )
                    , succeed Pred |= parsePred
                    ]
            )


isVarChar : Char -> Bool
isVarChar char =
    Char.isLower char
        || Char.isUpper char
        || Char.isDigit char
        || char
        == '_'


keywords : Set.Set String
keywords =
    Set.fromList [ "forall", "exists", "true", "false" ]


atom =
    oneOf
        [ succeed Top |. symbol "true"
        , succeed Bot |. symbol "false"
        ]


rat =
    succeed (\z1 z2 -> (Div z1 z2))
        |= Parser.int
        |. symbol "/"
        |= Parser.int


exprAtom =
    oneOf
        [ succeed (\x -> Var 0 x)
            |= (succeed VI
                    |= variable Char.isLower isVarChar keywords
               )
        , succeed ConstRat |= rat
        ]


infixArithmeticOp r =
    symbol "("
        |> andThen
            (\_ ->
                oneOf
                    [ delayedCommit (symbol "+")
                        (succeed
                            Plus
                            |. spaces
                            |= lazy r
                            |. spaces
                            |= lazy r
                            |. symbol ")"
                        )
                    , delayedCommit (symbol "-")
                        (succeed
                            Minus
                            |. spaces
                            |= lazy r
                            |. spaces
                            |= lazy r
                            |. symbol ")"
                        )
                    , delayedCommit (symbol "*")
                        (succeed
                            ConstFact
                            |. spaces
                            |= rat
                            |. spaces
                            |= lazy r
                            |. symbol ")"
                        )
                    ]
            )


parsePred =
    oneOf
        [ delayedCommit (symbol "<")
            (succeed
                Less
                |. spaces
                |= expr
                |. spaces
                |= expr
                |. symbol ")"
            )
        , delayedCommit (symbol "=")
            (succeed
                Eq
                |. spaces
                |= expr
                |. spaces
                |= expr
                |. symbol ")"
            )
        ]


prop =
    oneOf [ infixOp (\_ -> prop), atom ]


expr : Parser Expr
expr =
    oneOf [ infixArithmeticOp (\_ -> expr), exprAtom ]


deBruijnExp : List VarIdentifier -> Expr -> Maybe Expr
deBruijnExp ctx e =
  case e of
    Plus e1 e2 ->
      let
        (e1_, e2_) = (deBruijnExp ctx e1, deBruijnExp ctx e2)
      in
        case (e1_, e2_) of
          (Just e1__, Just e2__ ) -> Just (Plus e1__ e2__)
          (_, _) -> Nothing
    Minus e1 e2 ->
      let
        e1_ = deBruijnExp ctx e1
        e2_ = deBruijnExp ctx e2
      in
        case ( e1_, e2_ ) of
          (Just e1__, Just e2__) -> Just (Minus e1__ e2__)
          (_, _) -> Nothing
    Var _ s ->
      case Util.indexOf ctx s of
          Just i  -> Just (Var i s)
          Nothing -> Just (Var -1 s)
    ConstFact r e1 ->
      (deBruijnExp ctx e1)
          |> Maybe.andThen
              (\e1_ ->
                  Just (ConstFact r e1_)
              )
    e_ -> Just e_



-- Convert `RatPred`s to nameless representation.


deBruijnRP : List VarIdentifier -> RatPred -> Maybe RatPred
deBruijnRP ctx rp =
    case rp of
        Less e1 e2 ->
            case ( deBruijnExp ctx e1, deBruijnExp ctx e2 ) of
                ( Just e1_, Just e2_ ) ->
                    Just (Less e1_ e2_)

                ( _, _ ) ->
                    Nothing

        Greater e1 e2 ->
            case ( deBruijnExp ctx e1, deBruijnExp ctx e2 ) of
                ( Just e1_, Just e2_ ) ->
                    Just (Greater e1_ e2_)

                ( _, _ ) ->
                    Nothing

        Eq e1 e2 ->
            case ( deBruijnExp ctx e1, deBruijnExp ctx e2 ) of
                ( Just e1_, Just e2_ ) ->
                    Just (Eq e1_ e2_)

                ( _, _ ) ->
                    Nothing



-- Convert `Prop`s to nameless representation.


deBruijn : List VarIdentifier -> Prop -> Maybe Prop
deBruijn ctx p =
    case p of
        Pred rp ->
            case deBruijnRP ctx rp of
                Just rp_ ->
                    Just (Pred rp_)

                Nothing ->
                    Nothing

        Neg p ->
            (deBruijn ctx p) |> Maybe.andThen (\x -> Just (Neg x))

        Conj p1 p2 ->
            (deBruijn ctx p1)
                |> Maybe.andThen
                    (\p1_ ->
                        (deBruijn ctx p2)
                            |> Maybe.andThen
                                (\p2_ ->
                                    (Just (Conj p1_ p2_))
                                )
                    )

        Disj p1 p2 ->
            (deBruijn ctx p1)
                |> Maybe.andThen
                    (\p1_ ->
                        (deBruijn ctx p2)
                            |> Maybe.andThen
                                (\p2_ ->
                                    (Just (Disj p1_ p2_))
                                )
                    )

        Forall x p ->
            (deBruijn (x :: ctx) p)
                |> Maybe.andThen
                    (\p_ ->
                        Just (Forall x p_)
                    )

        Exists x p ->
            (deBruijn (x :: ctx) p)
                |> Maybe.andThen
                    (\p_ ->
                        Just (Exists x p_)
                    )

        p_ ->
            Just p_


substExpr : Expr -> Int -> Expr -> Expr
substExpr e i eNew =
    case e of
        Plus e1 e2 ->
            Plus (substExpr e1 i eNew) (substExpr e2 i eNew)

        Minus e1 e2 ->
            Plus (substExpr e1 i eNew) (substExpr e2 i eNew)

        Var i_ s ->
            if i == i_ then
                eNew
            else
                Var i s

        ConstRat r ->
            ConstRat r

        ConstFact r e1 ->
            ConstFact r (substExpr e1 i eNew)


subst : Prop -> Int -> Expr -> Prop
subst p i e =
    case p of
        Pred (Less e1 e2) ->
            Pred (Less (substExpr e1 i e) (substExpr e2 i e))

        Pred (Eq e1 e2) ->
            Pred (Eq (substExpr e1 i e) (substExpr e2 i e))

        Neg p_ ->
            Neg (subst p_ i e)

        Conj p1 p2 ->
            Conj (subst p1 i e) (subst p2 i e)

        Disj p1 p2 ->
            Disj (subst p1 i e) (subst p2 i e)

        Forall x p_ ->
            Forall x (subst p_ i e)

        Exists x p_ ->
            Exists x (subst p_ i e)

        p_ ->
            p_



-- Parse a given string into a proposition.


parseProp : String -> Maybe Prop
parseProp s =
    case run prop s of
        Ok p ->
            deBruijn [] p

        Err s ->
            Nothing
