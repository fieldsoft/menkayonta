module QueryParserTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser
import QueryParser exposing (Qs(..), Qstring(..), qstringP, qandP)
import Test exposing (..)


suite : Test
suite =
    describe "The QueryParser module"
        [ stringTests ]


andTests : Test
andTests =
    describe "Qs Qand"
        [ test "One unquoted string is Qand" <|
              \_ ->
              Expect.equal
              (Ok (Qand [Qstr "a"]))
              (Parser.run qandP "a")
        , test "Two unquoted strings are Qand" <|
              \_ ->
              Expect.equal
              (Ok (Qand [Qstr "a", Qstr "b"]))
              (Parser.run qandP "a b")
        ]
        

stringTests : Test
stringTests =
    describe "Qstring"
        [ test "A quoted string is a Qstring" <|
            \_ ->
                Expect.equal
                    (Ok (Qstr "a"))
                    (Parser.run qstringP "\"a\"")
        , test "A string in a quoted environment can have spaces" <|
            \_ ->
                Expect.equal
                    (Ok (Qstr "a b"))
                    (Parser.run qstringP "\"a b\"")
        , test "A string in a quoted environment can have an escaped quote" <|
            \_ ->
                Expect.equal
                    (Ok (Qstr "a\"b"))
                    (Parser.run qstringP "\"a\\\"b\"")
        , test "An unquoted string is a Qstring" <|
            \_ ->
                Expect.equal
                    (Ok (Qstr "a"))
                    (Parser.run qstringP "a")
        , test "A string in an unquoted environment can't have spaces" <|
            \_ ->
                Expect.equal
                    (Ok (Qstr "a"))
                    (Parser.run qstringP "a b")
        , test "A string in an unquoted environment can have an unescaped quote" <|
            \_ ->
                Expect.equal
                    (Ok (Qstr "a\"b"))
                    (Parser.run qstringP "a\"b")
        ]
