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
import Syntax exposing (Prop(..))


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
                    , succeed Neg
                        |. symbol "~"
                        |. spaces
                        |= lazy r
                        |. spaces
                        |. symbol ")"
                    ]
            )


atom =
    oneOf [ succeed Top |. symbol "true", succeed Bot |. symbol "false" ]


prop =
    oneOf [ infixOp (\_ -> prop), atom ]


parseProp : String -> Result Parser.Error Prop
parseProp s =
    run prop s
