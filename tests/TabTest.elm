module TabTest exposing (suite)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Tab exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Tab UI System"
        [ test "None does nothing" <|
            \_ ->
                Expect.equal
                    (initData Dict.empty)
                    (update None (initData Dict.empty) |> Tuple.first)
        ]
