module QueryParser exposing (Qs(..), Qstring(..), qandP, qstringP)

import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Step(..)
        , chompWhile
        , end
        , getChompedString
        , loop
        , map
        , oneOf
        , spaces
        , succeed
        , token
        )


type Qstring
    = Qstr String


type Qs
    = Qstring
    | Qand (List Qstring)


qandP : Parser Qs
qandP =
    succeed Qand
        |. spaces
        |= loop [] qstringsP


dumb : Parser (List String)
dumb =
    succeed 


qstringsP : List Qstring -> Parser (Step (List Qstring) (List Qstring))
qstringsP acc =
    oneOf
        [ spaces
            |> map (\_ -> Loop acc)
        , succeed (\qstr -> Loop (qstr :: acc))
            |= qstringP
        , succeed ()
            |> map (\_ -> Done (List.reverse acc))
        ]


qstringP : Parser Qstring
qstringP =
    succeed Qstr
        |= oneOf
            [ quotedStringP
            , unquotedStringP
            ]


unquotedStringP : Parser String
unquotedStringP =
    chompWhile (\char -> char /= ' ')
        |> getChompedString


quotedStringP : Parser String
quotedStringP =
    succeed identity
        |. token "\""
        |= loop [] quoteEnvStringP


quoteEnvStringP : List String -> Parser (Step (List String) String)
quoteEnvStringP acc =
    oneOf
        [ succeed (\chunk -> Loop (chunk :: acc))
            |. token "\\"
            |= oneOf
                [ map (\_ -> "\\") (token "\\")
                , map (\_ -> "\"") (token "\"")
                ]
        , token "\""
            |> map (\_ -> Done (String.join "" (List.reverse acc)))
        , chompWhile (\char -> char /= '"' && char /= '\\')
            |> getChompedString
            |> map (\chunk -> Loop (chunk :: acc))
        ]
