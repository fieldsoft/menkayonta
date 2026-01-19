module QueryParser exposing (Expr(..), parse)

import Parser
    exposing
        ( (|.)
        , (|=)
        , DeadEnd
        , Parser
        , Step(..)
        , andThen
        , chompWhile
        , getChompedString
        , lazy
        , loop
        , map
        , oneOf
        , run
        , spaces
        , succeed
        , symbol
        , token
        )


type Expr
    = Str String
    | Rx String
    | Or Expr Expr
    | And Expr Expr



-- | Qnot Qexpr
-- | Qfield String Qexpr


type Operator
    = OrOp
    | AndOp


parse : String -> Result (List DeadEnd) Expr
parse string =
    run expression string


term : Parser Expr
term =
    oneOf
        [ str
        , rx
        , succeed identity
            |. symbol "("
            |. spaces
            |= lazy (\_ -> expression)
            |. spaces
            |. symbol ")"
        ]


expression : Parser Expr
expression =
    term
        |> andThen (expression_ [])


expression_ : List ( Expr, Operator ) -> Expr -> Parser Expr
expression_ revOps expr =
    oneOf
        [ succeed Tuple.pair
            |. spaces
            |= operator
            |. spaces
            |= term
            |> andThen
                (\( op, newExpr ) ->
                    expression_ (( expr, op ) :: revOps) newExpr
                )
        , lazy (\_ -> succeed (finalize revOps expr))
        ]


operator : Parser Operator
operator =
    oneOf
        [ map (\_ -> OrOp) (symbol "or")
        , map (\_ -> AndOp) (symbol "and")
        ]


str : Parser Expr
str =
    succeed Str
        |. token "\""
        |= loop [] (quoteEnv '"')


rx : Parser Expr
rx =
    succeed Rx
        |. token "/"
        |= loop [] (quoteEnv '/')


quoteEnv : Char -> List String -> Parser (Step (List String) String)
quoteEnv q acc =
    oneOf
        [ succeed (\chunk -> Loop (chunk :: acc))
            |. token "\\"
            |= oneOf
                [ map (\_ -> "\\") (token "\\")
                , map
                    (\_ ->
                        String.fromChar q
                    )
                    (token (String.fromChar q))
                ]
        , token (String.fromChar q)
            |> map (\_ -> Done (String.join "" (List.reverse acc)))
        , chompWhile (\char -> char /= q && char /= '\\')
            |> getChompedString
            |> map (\chunk -> Loop (chunk :: acc))
        ]


finalize : List ( Expr, Operator ) -> Expr -> Expr
finalize revOps finalExpr =
    case revOps of
        [] ->
            finalExpr

        ( expr, AndOp ) :: otherRevOps ->
            finalize otherRevOps (And expr finalExpr)

        ( expr, OrOp ) :: otherRevOps ->
            Or (finalize otherRevOps expr) finalExpr
