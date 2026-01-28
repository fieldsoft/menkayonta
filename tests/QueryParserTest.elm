module QueryParserTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser
import QueryParser exposing (Expr(..), parse)
import Test exposing (..)


suite : Test
suite =
    describe "The QueryParser module"
        [ stringTests
        , andTests
        ]


andTests : Test
andTests =
    describe "Qs Qand"
        [ test "Two unquoted strings are Qand" <|
            \_ ->
                Expect.equal
                    (Ok (And (Str "a") (Str "b")))
                    (parse "\"a\" and \"b\"")
        ]


stringTests : Test
stringTests =
    describe "Qstring"
        [ test "A quoted string is a Qstring" <|
            \_ ->
                Expect.equal
                    (Ok (Str "a"))
                    (parse "\"a\"")
        , test "A string in a quoted environment can have spaces" <|
            \_ ->
                Expect.equal
                    (Ok (Str "a b"))
                    (parse "\"a b\"")
        , test "A string in a quoted environment can have an escaped quote" <|
            \_ ->
                Expect.equal
                    (Ok (Str "a\"b"))
                    (parse "\"a\\\"b\"")
        ]
