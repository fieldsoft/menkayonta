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
        , moveTests
        , closeTests
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
                        |> next (New simpleVentana)
                        |> Tuple.first
                        |> .counter
                    )
        , test "adds ventana to same column and row as focused" <|
            \_ ->
                Expect.equal
                    (Just simpleVentana)
                    (update (New simpleVentana) m
                        |> next (New simpleVentana)
                        |> Tuple.first
                        |> .ventanas
                        |> Dict.get ( 0, ( 0, 1 ) )
                    )
        , describe "when locked"
            [ test "preserves focus" <|
                \_ ->
                    Expect.equal
                        (Just ( 0, ( 0, 0 ) ))
                        (update (New simpleVentana) m
                            |> next (New simpleVentana)
                            |> Tuple.first
                            |> .focused
                        )
            , test "preserves focusHistory after one" <|
                \_ ->
                    Expect.equal
                        []
                        (update (New simpleVentana) m
                            |> next (New simpleVentana)
                            |> Tuple.first
                            |> .focusHistory
                        )
            , test "preserves focusHistory after several" <|
                \_ ->
                    Expect.equal
                        []
                        (update (New simpleVentana) m
                            |> next (New simpleVentana)
                            |> next (New simpleVentana)
                            |> next (New simpleVentana)
                            |> next (New simpleVentana)
                            |> next (New simpleVentana)
                            |> Tuple.first
                            |> .focusHistory
                        )
            ]
        , describe "when unlocked"
            [ test "sets focus if unlocked" <|
                \_ ->
                    Expect.equal
                        (Just ( 0, ( 0, 1 ) ))
                        (update (New simpleVentana) m
                            |> next Unlock
                            |> next (New simpleVentana)
                            |> Tuple.first
                            |> .focused
                        )
            , test "adds to focusHistory" <|
                \_ ->
                    Expect.equal
                        [ ( 0, ( 0, 0 ) ) ]
                        (update (New simpleVentana) m
                            |> next Unlock
                            |> next (New simpleVentana)
                            |> Tuple.first
                            |> .focusHistory
                        )
            , test "sets focusLock" <|
                \_ ->
                    Expect.equal
                        (Just ( 0, ( 0, 1 ) ))
                        (update (New simpleVentana) m
                            |> next Unlock
                            |> next (New simpleVentana)
                            |> Tuple.first
                            |> .focusLock
                        )
            ]
        ]


moveTests : Test
moveTests =
    describe "Moving a tab"
        [ describe "when there is a single tab"
            [ test "left has no effect for single item" <|
                \_ ->
                    Expect.equal
                        (update (New simpleVentana) m)
                        (update (New simpleVentana) m
                            |> next (Move Left)
                        )
            , test "right has no effect for single item" <|
                \_ ->
                    Expect.equal
                        (update (New simpleVentana) m)
                        (update (New simpleVentana) m
                            |> next (Move Right)
                        )
            , test "down has no effect for single item" <|
                \_ ->
                    Expect.equal
                        (update (New simpleVentana) m)
                        (update (New simpleVentana) m
                            |> next (Move Down)
                        )
            , test "up has no effect for single item" <|
                \_ ->
                    Expect.equal
                        (update (New simpleVentana) m)
                        (update (New simpleVentana) m
                            |> next (Move Up)
                        )
            ]
        , describe "when there are two tabs" <|
            [ test "left sets negative counter column and positive row" <|
                \_ ->
                    Expect.equal
                        (Just ( -2, ( 2, 0 ) ))
                        (update (New simpleVentana) m
                            |> next (New simpleVentana)
                            |> next (Move Left)
                            |> Tuple.first
                            |> .focused
                        )
            , test "right sets positive counter column and positive row" <|
                \_ ->
                    Expect.equal
                        (Just ( 2, ( 2, 0 ) ))
                        (update (New simpleVentana) m
                            |> next (New simpleVentana)
                            |> next (Move Right)
                            |> Tuple.first
                            |> .focused
                        )
            , test "down retains column and sets positive row" <|
                \_ ->
                    Expect.equal
                        (Just ( 0, ( 2, 0 ) ))
                        (update (New simpleVentana) m
                            |> next (New simpleVentana)
                            |> next (Move Down)
                            |> Tuple.first
                            |> .focused
                        )
            , test "up retains column and sets negative row" <|
                \_ ->
                    Expect.equal
                        (Just ( 0, ( -2, 0 ) ))
                        (update (New simpleVentana) m
                            |> next (New simpleVentana)
                            |> next (Move Up)
                            |> Tuple.first
                            |> .focused
                        )
            , test "left leaves focusHistory empty" <|
                \_ ->
                    Expect.equal
                        []
                        (update (New simpleVentana) m
                            |> next (New simpleVentana)
                            |> next (Move Left)
                            |> Tuple.first
                            |> .focusHistory
                        )
            ]
        ]


closeTests : Test
closeTests =
    describe "Closing a tab"
        [ describe "A visible unfocused tab"
            [ test "the nearest is visible" <|
                \_ ->
                    Expect.equal
                        (Just 4)
                        (update (New simpleVentana) m
                            -- 0 0 0
                            |> next (New simpleVentana)
                            -- 0 0 1
                            |> next (New simpleVentana)
                            -- 0 0 2
                            |> next (Move Down)
                            -- 0 3 0
                            |> next (New simpleVentana)
                            -- 0 3 4
                            |> next (New simpleVentana)
                            -- 0 3 5
                            |> next Unlock
                            |> next (Focus ( 0, ( 0, 1 ) ))
                            |> next (Close ( 0, ( 3, 0 ) ))
                            |> Tuple.first
                            |> .visVentanas
                            |> Dict.get ( 0, 3 )
                        )
            ]
        , describe "A non-visible tab"
            [ test "it closes a single tab" <|
                \_ ->
                    Expect.equal
                        Dict.empty
                        (update (New simpleVentana) m
                            |> next (Close ( 0, ( 0, 0 ) ))
                            |> Tuple.first
                            |> .ventanas
                        )
            , test "focus does not change" <|
                \_ ->
                    Expect.equal
                        (Just ( 0, ( 0, 0 ) ))
                        (update (New simpleVentana) m
                            |> next (New simpleVentana)
                            |> next (New simpleVentana)
                            |> next (New simpleVentana)
                            |> next (New simpleVentana)
                            |> next (Close ( 0, ( 0, 2 ) ))
                            |> Tuple.first
                            |> .focused
                        )
            , test "the focused tab is visible" <|
                \_ ->
                    Expect.equal
                        (Just 0)
                        (update (New simpleVentana) m
                            |> next (New simpleVentana)
                            |> next (New simpleVentana)
                            |> next (New simpleVentana)
                            |> next (New simpleVentana)
                            |> next (Close ( 0, ( 0, 2 ) ))
                            |> Tuple.first
                            |> .visVentanas
                            |> Dict.get ( 0, 0 )
                        )
            ]
        , describe "two rows"
            [ describe "no focus history"
                [ test "there is no focus history before close" <|
                    \_ ->
                        Expect.equal
                            -- The nearest should be in the same row
                            -- unless the row is now empty.
                            []
                            (update (New simpleVentana) m
                                -- 0 0 0
                                |> next (New simpleVentana)
                                -- 0 0 1
                                |> next (New simpleVentana)
                                -- 0 0 2
                                |> next (Move Down)
                                -- 0 3 0
                                |> next (New simpleVentana)
                                -- 0 3 4
                                |> next (New simpleVentana)
                                -- 0 3 5
                                |> Tuple.first
                                |> .focusHistory
                            )
                , test "the nearest is focused" <|
                    \_ ->
                        Expect.equal
                            -- The nearest should be in the same row
                            -- unless the row is now empty.
                            (Just ( 0, ( 3, 4 ) ))
                            (update (New simpleVentana) m
                                -- 0 0 0
                                |> next (New simpleVentana)
                                -- 0 0 1
                                |> next (New simpleVentana)
                                -- 0 0 2
                                |> next (Move Down)
                                -- 0 3 0
                                |> next (New simpleVentana)
                                -- 0 3 4
                                |> next (New simpleVentana)
                                -- 0 3 5
                                |> next (Close ( 0, ( 3, 0 ) ))
                                |> Tuple.first
                                |> .focused
                            )
                , test "the nearest is visible" <|
                    \_ ->
                        Expect.equal
                            (Just 4)
                            (update (New simpleVentana) m
                                -- 0 0 0 (focused)
                                |> next (New simpleVentana)
                                -- 0 0 1
                                |> next (New simpleVentana)
                                -- 0 0 2
                                |> next (Move Down)
                                -- 0 3 0 (focused) (0 0 1 visible)
                                |> next (New simpleVentana)
                                -- 0 3 4 (nearest)
                                |> next (New simpleVentana)
                                -- 0 3 5
                                |> next (Close ( 0, ( 3, 0 ) ))
                                |> Tuple.first
                                |> .visVentanas
                                |> Dict.get ( 0, 3 )
                            )
                ]
            , describe "with focus history"
                [ test "the last is focused" <|
                    \_ ->
                        Expect.equal
                            (Just ( 0, ( 3, 0 ) ))
                            (update (New simpleVentana) m
                                |> next (New simpleVentana)
                                |> next (New simpleVentana)
                                |> next (Move Down)
                                |> next Unlock
                                |> next (Focus ( 0, ( 0, 1 ) ))
                                |> next (New simpleVentana)
                                |> next (New simpleVentana)
                                |> next (Close ( 0, ( 0, 1 ) ))
                                |> Tuple.first
                                |> .focused
                            )
                , test "the nearest is visible" <|
                    \_ ->
                        Expect.equal
                            (Just 2)
                            (update (New simpleVentana) m
                                -- 0 0 0
                                |> next (New simpleVentana)
                                -- 0 0 1
                                |> next (New simpleVentana)
                                -- 0 0 2
                                |> next (Move Down)
                                -- 0 3 0
                                |> next (New simpleVentana)
                                -- 0 3 4
                                |> next (New simpleVentana)
                                -- 0 3 5
                                |> next Unlock
                                |> next (Focus ( 0, ( 0, 1 ) ))
                                |> next (Close ( 0, ( 0, 1 ) ))
                                |> Tuple.first
                                |> .visVentanas
                                |> Dict.get ( 0, 0 )
                            )
                , test "the focused is visible" <|
                    \_ ->
                        Expect.equal
                            (Just 0)
                            (update (New simpleVentana) m
                                |> next (New simpleVentana)
                                |> next (New simpleVentana)
                                |> next (Move Down)
                                |> next (New simpleVentana)
                                |> next (New simpleVentana)
                                |> next Unlock
                                |> next (Focus ( 0, ( 0, 1 ) ))
                                |> next (Close ( 0, ( 0, 1 ) ))
                                |> Tuple.first
                                |> .visVentanas
                                |> Dict.get ( 0, 3 )
                            )
                ]
            ]
        ]


next : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
next msg ( model, _ ) =
    update msg model
