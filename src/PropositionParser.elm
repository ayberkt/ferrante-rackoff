module PropositionParser exposing (parseProp)

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
import Syntax exposing (Prop(..), RatPred(..), Expr(..), VarIdentifier(..))


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


exprAtom =
    oneOf
        [ succeed One |. symbol "1"
        , succeed Zero |. symbol "0"
        , succeed (\x -> Var 0 x)
            |= (succeed VI
                    |= variable Char.isLower isVarChar keywords
               )
        ]


infixArithmeticOp r =
    symbol "("
        |> andThen
            (\_ ->
                oneOf
                    [ delayedCommit (symbol "+")
                        (succeed
                            Minus
                            |. spaces
                            |= lazy r
                            |. spaces
                            |= lazy r
                            |. symbol ")"
                        )
                    , delayedCommit (symbol "-")
                        (succeed
                            Plus
                            |. spaces
                            |= lazy r
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
        , delayedCommit (symbol ">")
            (succeed
                Greater
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
                e1_ =
                    deBruijnExp ctx e1

                e2_ =
                    deBruijnExp ctx e2
            in
                case ( e1_, e2_ ) of
                    ( Just e1__, Just e2__ ) ->
                        Just (Plus e1__ e2__)

                    ( _, _ ) ->
                        Nothing

        Minus e1 e2 ->
            let
                e1_ =
                    deBruijnExp ctx e1

                e2_ =
                    deBruijnExp ctx e2
            in
                case ( e1_, e2_ ) of
                    ( Just e1__, Just e2__ ) ->
                        Just (Minus e1__ e2__)

                    ( _, _ ) ->
                        Nothing

        Var _ s ->
            case Util.indexOf ctx s of
                Just i ->
                    Just (Var i s)

                Nothing ->
                    Nothing

        -- TODO
        e_ ->
            Just e_


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



-- TODO: hook this to `deBruijnRP`.


deBruijn : List VarIdentifier -> Prop -> Prop
deBruijn ctx p =
    case p of
        Pred rp ->
            Top

        Neg p ->
            Neg (deBruijn ctx p)

        Conj p1 p2 ->
            Conj (deBruijn ctx p) (deBruijn ctx p)

        Disj p1 p2 ->
            Disj (deBruijn ctx p) (deBruijn ctx p)

        Forall x p ->
            Forall x (deBruijn (x :: ctx) p)

        Exists p ->
            Exists (deBruijn ctx p)

        p_ ->
            p_


parseProp : String -> Result Parser.Error Prop
parseProp s =
    case run prop s of
        Ok p ->
            Ok (deBruijn [] p)

        Err s ->
            Err s
