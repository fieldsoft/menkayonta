module Tab exposing
    ( Direction(..)
    , Model
    , Msg(..)
    , Param(..)
    , Path
    , Ventana
    , VentanaParams
    , Ventanas
    , VisVentanas
    , Vista
    , Vistas
    , defVParams
    , getByVista
    , initData
    , tabpath
    , tpToS
    , treeifyTabs
    , update
    , visMember
    )

import Browser.Dom as Dom
import Content exposing (Content)
import Dict exposing (Dict)
import List.Extra as LE
import Math.Vector3 as V3
import Maybe.Extra as ME
import Set exposing (Set)
import Task


type Msg
    = Clone
    | Close
    | Focus Path
    | Goto Path
    | Move Direction
    | New Ventana
    | None
    | Change Param Path String


{-| Inject a message into `Cmd`
-}
sendMsg : Msg -> Cmd Msg
sendMsg msg =
    Task.succeed msg
        |> Task.perform identity


type alias Model =
    { counter : Int
    , ventanas : Ventanas
    , focused : Maybe Path
    , visVentanas : VisVentanas
    , vistas : Vistas
    , globalVistas : List String
    }


type Param
    = Search
    | Length
      

{-| A Ventana supplies the title and a referrence to a Vista, which is
an identifier for some viewable content. I use Spanish when there are
already commonly referred to object or concepts such as "window" or
"view".
-}
type alias Ventana =
    { title : String
    , fullTitle : String
    , vista : String
    , params : VentanaParams
    }


type alias VentanaParams =
    { length : Int
    , searchString : String
    }


{-| All of the viewable content associated with a tab.
-}
type alias Ventanas =
    Dict Path Ventana


{-| Viewable content.
-}
type alias Vista =
    { project : String
    , kind : String
    , identifier : String
    , content : Content
    }


type alias Vistas =
    Dict String Vista


{-| The path to a tab, used for operations on tabs.
-}
type alias Path =
    ( Int, ( Int, Int ) )


{-| Currently visible Ventanas.
-}
type alias VisVentanas =
    Dict ( Int, Int ) Int


type Direction
    = Left
    | Right
    | Up
    | Down


defVParams : VentanaParams
defVParams =
    { length = 0
    , searchString = ""
    }


initData : Dict String Vista -> Model
initData globalVistas =
    { counter = 0
    , ventanas = Dict.empty
    , focused = Nothing
    , visVentanas = Dict.empty
    , vistas = globalVistas
    , globalVistas = Dict.keys globalVistas
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        Change Search tp str ->
            case Dict.get tp model.ventanas of
                Just ventana ->
                    let
                        params =
                            ventana.params

                        np =
                            { params | searchString = str }

                        nv =
                            { ventana | params = np }

                        nvs =
                            Dict.insert tp nv model.ventanas
                    in
                    ( { model | ventanas = nvs }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )
            
                
        Change Length tp str ->
            let
                num =
                    Maybe.withDefault 0 (String.toInt str)
            in
            case Dict.get tp model.ventanas of
                Just ventana ->
                    let
                        params =
                            ventana.params

                        np =
                            { params | length = num }

                        nv =
                            { ventana | params = np }

                        nvs =
                            Dict.insert tp nv model.ventanas
                    in
                    ( { model | ventanas = nvs }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )
            
        New ventana ->
            if Dict.isEmpty model.ventanas then
                let
                    c =
                        model.counter

                    tp =
                        tabpath c c c

                    newmodel =
                        { model
                            | counter = c + 1
                            , ventanas = Dict.singleton tp ventana
                            , visVentanas = visInsert tp Dict.empty
                            , focused = Just tp
                        }
                in
                ( newmodel, Cmd.none )

            else
                let
                    c =
                        model.counter

                    {- The focused property is used to get the column
                       and row that the user has been interacting
                       with. It is wrapped in a Maybe. It should not be
                       Nothing unless there are no ventanas, which
                       should not be the case in this 'else' block. In
                       the unlikely case of a bad state, the input model
                       is returned. Perhaps an error would be better.
                    -}
                    newmodel =
                        case model.focused of
                            Nothing ->
                                model

                            Just tp1 ->
                                let
                                    ( column, ( row, _ ) ) =
                                        tp1

                                    tp2 =
                                        tabpath column row c
                                in
                                { model
                                    | counter =
                                        c + 1
                                    , ventanas =
                                        Dict.insert tp2 ventana model.ventanas
                                    , visVentanas =
                                        visInsert tp2 model.visVentanas
                                    , focused =
                                        Just tp2
                                }
                in
                ( newmodel, Cmd.none )

        Goto tp ->
            let
                id =
                    tpToS tp
            in
            ( model
            , Cmd.batch
                [ Dom.getElement id
                    |> Task.andThen
                        (\el ->
                            Dom.setViewport el.element.x el.element.y
                        )
                    |> Task.attempt (\_ -> None)
                , sendMsg (Focus tp)
                ]
            )

        Focus tp ->
            let
                title =
                    Dict.get tp model.ventanas
                        |> Maybe.map .title
                        |> Maybe.withDefault ""
            in
            ( { model
                | focused = Just tp
                , visVentanas = visInsert tp model.visVentanas
              }
            , Cmd.none
            )

        Close ->
            let
                tp =
                    Maybe.withDefault (tabpath -1 -1 -1) <| model.focused
            in
            ( closeTab True tp model, Cmd.none )

        Clone ->
            let
                ventana =
                    model.focused
                        |> Maybe.andThen
                            (\tp -> Dict.get tp model.ventanas)
            in
            case ventana of
                Nothing ->
                    ( model, Cmd.none )

                Just v ->
                    ( model, sendMsg (New v) )

        Move dir ->
            {- If there is more than one tab, and one is focused,
               which always should be the case, see if there is more
               than one tab in the row of the focused tab. If there is
               not, the row will close when the focused tab is
               removed. If there is, ensure that the nearest tab is
               made visible. If there is no column or row in the
               target direction, add a new column and/or row and
               create a tab that references the same ventana as the
               previous item. If there is a row at the target site,
               create a new tab there, again referencing the original
               ventana. Delete the original focused item and focus the
               new tab.
            -}
            let
                vs =
                    model.ventanas

                c =
                    model.counter

                keys =
                    Dict.keys vs

                cols =
                    tcolumns keys

                newmodel =
                    if Dict.isEmpty vs || Dict.size vs == 1 then
                        model

                    else
                        case model.focused of
                            Just fp ->
                                let
                                    -- rows of the current column
                                    rows =
                                        trows (tcolumn fp) keys
                                in
                                {- Does the position of the tab's
                                   column or row require that a new
                                   tab or column be added to supply a
                                   target?
                                -}
                                if createNecessary dir fp ( cols, rows ) then
                                    let
                                        newtp =
                                            newTabPath dir fp c

                                        moved =
                                            reassign fp newtp model
                                    in
                                    { moved | counter = c + 1 }

                                else
                                    let
                                        new =
                                            insertTabPath dir fp ( cols, rows ) keys
                                    in
                                    reassign fp new model

                            Nothing ->
                                model
            in
            ( newmodel, Cmd.none )


{-| insertTab.Path, newTab.Path, and createNecessary are all helpers for
Move Direction. Each provides Direction specific code for some
aspect of the Move operation. This is for the case when movement
places the focused tab in a preexisting row with tabs.
-}
insertTabPath : Direction -> Path -> ( List Int, List Int ) -> List Path -> Path
insertTabPath dir tp ( cols, rows ) keys =
    case dir of
        Right ->
            let
                col =
                    getNext (tcolumn tp) cols

                newrows =
                    trows col keys
            in
            List.head newrows
                |> Maybe.map (\r -> tabpath col r (ttab tp))
                |> Maybe.withDefault tp

        Left ->
            let
                col =
                    getNext (tcolumn tp) (List.reverse cols)

                newrows =
                    trows col keys
            in
            List.head newrows
                |> Maybe.map (\r -> tabpath col r (ttab tp))
                |> Maybe.withDefault tp

        Down ->
            let
                row =
                    getNext (trow tp) rows
            in
            tabpath (tcolumn tp) row (ttab tp)

        Up ->
            let
                row =
                    getNext (trow tp) (List.reverse rows)
            in
            tabpath (tcolumn tp) row (ttab tp)


{-| Use the counter (c) to provide new Tab.Paths that will be rendered
below, above, to the left or right of the focused tab.
-}
newTabPath : Direction -> Path -> Int -> Path
newTabPath dir tp c =
    case dir of
        Right ->
            tabpath c c (ttab tp)

        Left ->
            tabpath (negate c) c (ttab tp)

        Down ->
            tabpath (tcolumn tp) c (ttab tp)

        Up ->
            tabpath (tcolumn tp) (negate c) (ttab tp)


{-| Essentially check the heads and tails of the relevant lists of
columns or rows to determine if the current item is on the border
of a column or row structure.
-}
createNecessary : Direction -> Path -> ( List Int, List Int ) -> Bool
createNecessary dir tp ( cols, rows ) =
    case dir of
        Right ->
            Just (tcolumn tp) == LE.last cols

        Left ->
            Just (tcolumn tp) == List.head cols

        Down ->
            Just (trow tp) == LE.last rows

        Up ->
            Just (trow tp) == List.head rows


{-| Return the Tab.Paths for the tabs in the same row.
-}
sharesRow : Path -> Model -> List Path
sharesRow tp model =
    let
        matchrow tp2 =
            trow tp == trow tp2 && tp /= tp2
    in
    List.filter matchrow (Dict.keys model.ventanas)


{-| This uses vector distance to find a new focused item. It is called
in instances such as the closing of a tab. The idea is that some
tab should become open and focused when the focused one
closes. Intuitively, this should be the one that is nearest the
closed tab.
-}
nearest : Path -> Model -> Maybe Path
nearest tp model =
    let
        toV3 ( column, ( row, tab ) ) =
            V3.vec3 (toFloat column) (toFloat row) (toFloat tab)

        vp =
            toV3 tp

        nearest_ tps =
            List.map (\t -> ( V3.distance (toV3 t) vp, t )) tps
                |> List.minimum
                |> Maybe.map Tuple.second
                |> (\t ->
                        if t == Just tp then
                            Nothing

                        else
                            t
                   )
    in
    case sharesRow tp model of
        [] ->
            nearest_ (visRemove tp model.visVentanas |> visToList)

        tps ->
            nearest_ tps


{-| This will close a tab and set the nearest tab to focused.
-}
closeTab : Bool -> Path -> Model -> Model
closeTab closevista tp model =
    let
        gvistas =
            model.globalVistas

        vista =
            Dict.get tp model.ventanas
                |> Maybe.map .vista
                |> Maybe.withDefault "fake"

        multiref =
            getAllByVista vista model.ventanas
                |> List.length
                |> (<) 1

        nonglobal =
            not <| List.member vista gvistas

        vistas =
            if nonglobal && not multiref && closevista then
                Dict.remove vista model.vistas

            else
                model.vistas

        notClosed =
            visRemove tp model.visVentanas

        focused =
            nearest tp model

        visVentanas =
            ME.unwrap notClosed
                (\f -> visInsert f notClosed)
                focused
    in
    { model
        | ventanas = Dict.remove tp model.ventanas
        , focused = focused
        , visVentanas = visVentanas
        , vistas = vistas
    }


closeAll : String -> Model -> Model
closeAll vista model =
    getAllByVista vista model.ventanas
        |> List.foldl (closeTab False) model


{-| Assign a ventana to a new tab.
-}
reassign : Path -> Path -> Model -> Model
reassign old new model =
    let
        closed =
            closeTab False old model

        ventanas =
            Dict.get old model.ventanas
                |> Maybe.map (\v -> Dict.insert new v closed.ventanas)
                |> Maybe.withDefault closed.ventanas

        visVentanas =
            visInsert new closed.visVentanas
    in
    { closed
        | ventanas = ventanas
        , visVentanas = visVentanas
        , focused = Just new
    }


{-| Attempts to find the integer after the current one in a list and
returns the current integer on failure.
-}
getNext : Int -> List Int -> Int
getNext curr others =
    LE.splitWhen (\x -> x == curr) others
        |> Maybe.map Tuple.second
        |> Maybe.andThen List.tail
        |> Maybe.andThen List.head
        |> Maybe.withDefault curr


{-| Create a tree structure from the flat path listing of Tab.Paths to
be used by the view function.
-}
treeifyTabs : List Path -> Dict Int (Dict Int (Set Int))
treeifyTabs tps =
    List.foldl
        (\tp tr ->
            case Dict.get (tcolumn tp) tr of
                Nothing ->
                    let
                        newtabs =
                            Set.singleton (ttab tp)

                        newrows =
                            Dict.singleton (trow tp) newtabs
                    in
                    Dict.insert (tcolumn tp) newrows tr

                Just rows ->
                    case Dict.get (trow tp) rows of
                        Nothing ->
                            let
                                newtabs =
                                    Set.singleton (ttab tp)

                                newrows =
                                    Dict.insert (trow tp) newtabs rows
                            in
                            Dict.insert (tcolumn tp) newrows tr

                        Just tabs ->
                            if not (Set.member (ttab tp) tabs) then
                                let
                                    newtabs =
                                        Set.insert (ttab tp) tabs

                                    newrows =
                                        Dict.insert (trow tp) newtabs rows
                                in
                                Dict.insert (tcolumn tp) newrows tr

                            else
                                -- Unexpected
                                tr
        )
        Dict.empty
        tps



{- Tab.Path helper functions are mostly used to help document the
   intention of working with the integer tuple.
-}


tabpath : Int -> Int -> Int -> Path
tabpath c r t =
    ( c, ( r, t ) )


tpToS : Path -> String
tpToS ( c, ( r, t ) ) =
    [ c, r, t ]
        |> List.map String.fromInt
        |> String.join ","


tcolumn : Path -> Int
tcolumn tp =
    Tuple.first tp


trow : Path -> Int
trow tp =
    tp |> Tuple.second |> Tuple.first


ttab : Path -> Int
ttab tp =
    tp |> Tuple.second |> Tuple.second


{-| All columns in order
-}
tcolumns : List Path -> List Int
tcolumns tps =
    List.map tcolumn tps
        |> LE.unique
        |> List.sort


{-| Rows for a column in order
-}
trows : Int -> List Path -> List Int
trows column tps =
    List.filter (\tp -> tcolumn tp == column) tps
        |> List.map trow
        |> LE.unique
        |> List.sort


{-| Insert a tab into VisVentanas
-}
visInsert : Path -> VisVentanas -> VisVentanas
visInsert tp vv =
    Dict.insert ( tcolumn tp, trow tp ) (ttab tp) vv


{-| Insert a tab into VisVentanas
-}
visRemove : Path -> VisVentanas -> VisVentanas
visRemove tp vv =
    Dict.remove ( tcolumn tp, trow tp ) vv


visToList : VisVentanas -> List Path
visToList vv =
    Dict.toList vv
        |> List.map (\( ( c, r ), t ) -> ( c, ( r, t ) ))


visMember : Path -> VisVentanas -> Bool
visMember ( col, ( row, tab ) ) vv =
    case Dict.get ( col, row ) vv of
        Nothing ->
            False

        Just t ->
            ( col, ( row, tab ) ) == ( col, ( row, t ) )


getVistaVentana : Path -> Model -> Maybe ( Vista, Ventana )
getVistaVentana tp model =
    Dict.get tp model.ventanas
        |> Maybe.andThen
            (\ventana ->
                Dict.get ventana.vista model.vistas
                    |> Maybe.map (\vista -> ( vista, ventana ))
            )


getContentVistaVentana : Path -> Model -> Maybe ( Content, ( Vista, Ventana ) )
getContentVistaVentana tp model =
    getVistaVentana tp model
        |> Maybe.map (\( vis, ven ) -> ( vis.content, ( vis, ven ) ))


getContentVistaFromVistas : String -> Vistas -> Maybe ( Content, Vista )
getContentVistaFromVistas vid vistas =
    Dict.get vid vistas
        |> Maybe.map (\vista -> ( vista.content, vista ))


getByVista : String -> Dict Path Ventana -> Maybe Path
getByVista vista ventanas =
    List.head <| getAllByVista vista ventanas


getAllByVista : String -> Dict Path Ventana -> List Path
getAllByVista vista ventanas =
    -- TODO use Dict.filter (this may not warrant being a helper
    -- function.)
    List.filter (\( _, v ) -> v.vista == vista) (Dict.toList ventanas)
        |> List.map Tuple.first
