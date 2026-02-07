module TabTest exposing (suite)

import Content
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Form.Global
import Tab exposing (..)
import Test exposing (..)


simpleContent : Form.Global.Model
simpleContent =
    Form.Global.initData


simpleVista : Vista
simpleVista =
    { project = "global"
    , kind = "global"
    , identifier = "global"
    , content = Content.GF simpleContent
    }


simpleVentana : Ventana
simpleVentana =
    { title = "Simple Ventana"
    , fullTitle = "Simple Ventana"
    , vista = "global"
    , params = { length = 0, searchString = "" }
    }


m : Model
m =
    initData Dict.empty


suite : Test
suite =
    describe "Tab UI System"
        [ test "None does nothing" <|
            \_ ->
                Expect.equal
                    m
                    (update None m |> Tuple.first)
        , test "Goto doesn't change the model" <|
            \_ ->
                Expect.equal
                    m
                    (update (Goto ( 0, ( 0, 0 ) )) m |> Tuple.first)
        , newTests
        ]


newTests : Test
newTests =
    describe "New Ventana"
        [ newOnEmpty
        , newOnNonEmpty
        ]


newOnEmpty : Test
newOnEmpty =
    describe "New on empty"
        [ test "updates counter" <|
            \_ ->
                Expect.equal
                    (m.counter + 1)
                    (update (New simpleVentana) m
                        |> Tuple.first
                        |> .counter
                    )
        , test "adds a ventana" <|
            \_ ->
                Expect.equal
                    (Just simpleVentana)
                    (update (New simpleVentana) m
                        |> Tuple.first
                        |> .ventanas
                        |> Dict.get ( 0, ( 0, 0 ) )
                    )
        , test "makes a ventana visible" <|
            \_ ->
                Expect.equal
                    (Just 0)
                    (update (New simpleVentana) m
                        |> Tuple.first
                        |> .visVentanas
                        |> Dict.get ( 0, 0 )
                    )
        , test "has a blank focusHistory" <|
            \_ ->
                Expect.equal
                    []
                    (update (New simpleVentana) m
                        |> Tuple.first
                        |> .focusHistory
                    )
        , test "locks the focus" <|
            \_ ->
                Expect.equal
                    (Just ( 0, ( 0, 0 ) ))
                    (update (New simpleVentana) m
                        |> Tuple.first
                        |> .focusLock
                    )
        ]


newOnNonEmpty : Test
newOnNonEmpty =
    describe "New on non-empty"
        [ test "updates counter" <|
            \_ ->
                Expect.equal
                    2
                    (update (New simpleVentana) m
                        |> (\m2 ->
                                update (New simpleVentana) (Tuple.first m2)
                           )
                        |> Tuple.first
                        |> .counter
                    )
        , test "adds ventana to same column and row as focused" <|
            \_ ->
                Expect.equal
                    (Just simpleVentana)
                    (update (New simpleVentana) m
                        |> (\m2 ->
                                update (New simpleVentana) (Tuple.first m2)
                           )
                        |> Tuple.first
                        |> .ventanas
                        |> Dict.get ( 0, ( 0, 1 ) )
                    )
        , test "preserves focus if locked" <|
            \_ ->
                Expect.equal
                    (Just ( 0, ( 0, 0 ) ))
                    (update (New simpleVentana) m
                        |> Tuple.first
                        |> update (New simpleVentana)
                        |> Tuple.first
                        |> .focused
                    )
        , test "sets focus if unlocked" <|
            \_ ->
                Expect.equal
                    (Just ( 0, ( 0, 1 ) ))
                    (update (New simpleVentana) m
                        |> Tuple.first
                        |> update Unlock
                        |> Tuple.first
                        |> update (New simpleVentana)
                        |> Tuple.first
                        |> .focused
                    )
        , test "adds to focusHistory" <|
            \_ ->
                Expect.equal
                    [ ( 0, ( 0, 0 ) ) ]
                    (update (New simpleVentana) m
                        |> Tuple.first
                        |> update Unlock
                        |> Tuple.first
                        |> update (New simpleVentana)
                        |> Tuple.first
                        |> .focusHistory
                    )
        , test "sets focusLock" <|
            \_ ->
                Expect.equal
                    (Just ( 0, ( 0, 1 ) ))
                    (update (New simpleVentana) m
                        |> Tuple.first
                        |> update Unlock
                        |> Tuple.first
                        |> update (New simpleVentana)
                        |> Tuple.first
                        |> .focusLock
                    )
        ]
