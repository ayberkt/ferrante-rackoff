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
import Syntax exposing (Prop(..), RatPred(..), Expr(..))


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
                            |= variable Char.isLower isVarChar keywords
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
        , succeed (\x -> Var 0 x) |= variable Char.isLower isVarChar keywords
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


parseProp : String -> Result Parser.Error Prop
parseProp s =
    run prop s
