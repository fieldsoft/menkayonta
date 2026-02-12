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
    , columnCount
    , multipleRows
    , defVParams
    , getByVista
    , initData
    , pathToString
    , tabpath
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
    | Close Path
    | Focus Path
    | Goto Path
    | Move Direction
    | New Ventana
    | None
    | Change Param Path String
    | Select Path
    | Unlock


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
    , focusHistory : List Path
    , focusLock : Maybe Path
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


{-| There is too much overhead in managing some types of information
if the tab can be cloned. It is better to simply forbid some kinds of
cloning. These are matched against vista identifiers.
-}
dontClone : List String
dontClone =
    [ "FORM:" ]


initData : Dict String Vista -> Model
initData globalVistas =
    { counter = 0
    , ventanas = Dict.empty
    , focused = Nothing
    , visVentanas = Dict.empty
    , vistas = globalVistas
    , globalVistas = Dict.keys globalVistas
    , focusHistory = []
    , focusLock = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        Unlock ->
            ( { model | focusLock = Nothing }
            , Cmd.none
            )

        Select tp ->
            case Dict.get tp model.ventanas of
                Nothing ->
                    ( model, Cmd.none )

                Just _ ->
                    ( focus tp model, Cmd.none )

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
            let
                c =
                    model.counter
            in
            case model.focused of
                Nothing ->
                    let
                        tp =
                            tabpath c c c

                        newmodel =
                            { model
                                | counter = c + 1
                                , ventanas = Dict.singleton tp ventana
                                , visVentanas = visInsert tp Dict.empty
                            }
                    in
                    ( focusWhenUnlocked tp newmodel, Cmd.none )

                Just focused ->
                    let
                        ( column, ( row, _ ) ) =
                            focused
                                
                        tp =
                            tabpath column row c

                        newmodel =
                            { model
                                | counter =
                                  c + 1
                                , ventanas =
                                  Dict.insert tp ventana model.ventanas
                            }
                    in
                    ( focusWhenUnlocked tp newmodel, Cmd.none )

        -- Goto triggers a viewport operation so that a tab is visible.
        Goto tp ->
            ( model
            , Dom.getElement (pathToString tp)
                |> Task.andThen
                    (\el ->
                        Dom.setViewport el.element.x el.element.y
                    )
                |> Task.attempt (\_ -> None)
            )

        Focus tp ->
            ( focusWhenUnlocked tp model, Cmd.none )

        Close tp ->
            ( closeTab True tp model, Cmd.none )

        Clone ->
            let
                ventana =
                    model.focused
                        |> Maybe.andThen
                            (\tp -> Dict.get tp model.ventanas)

                idPatternMatch ventana_ =
                    List.any
                        (\x -> String.startsWith x ventana_.vista)
                        dontClone

                noClone ventana_ =
                    idPatternMatch ventana_
                        || List.member ventana_.vista model.globalVistas
            in
            case ventana of
                Nothing ->
                    ( model, Cmd.none )

                Just v ->
                    if noClone v then
                        ( model, Cmd.none )

                    else
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

                                    needsCreation =
                                        isCreationNecessary dir fp ( cols
                                                                   , rows
                                                                   )
                                in
                                {- Does the position of the tab's
                                   column or row require that a new
                                   tab or column be added to supply a
                                   target?
                                -}
                                if needsCreation then
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


{-| insertTabPath, newTabPath, and isCreationNecessary are all helpers
for Move Direction. Each provides Direction specific code for some
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


{-| Use the counter, `c`, to provide new Paths that will be rendered
below, above, to the left or right of the focused tab. The row value
is always changed to ensure
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
isCreationNecessary : Direction -> Path -> ( List Int, List Int ) -> Bool
isCreationNecessary dir tp ( cols, rows ) =
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
sharesRow : Path -> List Path -> List Path
sharesRow tp tabs =
    let
        tabs_ =
            LE.remove tp tabs

        ( column, ( row, tab ) ) =
            tp
    in
    List.filter (sameRow tp) tabs_


sameRow : Path -> Path -> Bool
sameRow ( c1, ( r1, _ ) ) ( c2, ( r2, _ ) ) =
    c1 == c2 && r1 == r2


{-| This uses vector distance to find the tab nearest another. It is
called in instances such as the closing of a tab. The idea is that
some tab should become open and/or focused when another is
closed. Intuitively, this should be the one that is nearest the closed
tab.
-}
nearest : Path -> List Path -> Maybe Path
nearest tp tabs =
    let
        -- ensure the path is not in the list
        tabs_ =
            LE.remove tp tabs

        -- a function to convert a path to a vector
        toV3 ( column, ( row, tab ) ) =
            V3.vec3 (toFloat column) (toFloat row) (toFloat tab)

        -- the provided path as a vector
        vp =
            toV3 tp

        -- a function to find the tab that is least distant
        nearest_ tps =
            List.map (\t -> ( V3.distance (toV3 t) vp, t )) tps
                |> List.minimum
                |> Maybe.map Tuple.second
    in
    case sharesRow tp tabs_ of
        [] ->
            nearest_ tabs_

        -- There is a preference for remaining in the same row.
        tps ->
            nearest_ tps


{-| This will close a tab. If it was focused, it will focus the
previous tab in the history, or the nearest tab, otherwise.
-}
closeTab : Bool -> Path -> Model -> Model
closeTab closevista tp model =
    let
        ( column, ( row, tab ) ) =
            tp

        -- Was the tab a visible tab?
        wasVisible =
            Just tab == Dict.get ( column, row ) model.visVentanas

        -- Was the tab focused?
        wasFocused =
            model.focused == Just tp

        -- ventans without closed tab
        ventanas =
            Dict.remove tp model.ventanas

        -- The history with the closed tab excluded.
        newHistory =
            List.filter
                (\t -> t /= tp)
                (refreshHistory model.focusHistory (Dict.keys ventanas))

        -- The closest tab to the closed tab.
        near =
            nearest tp (Dict.keys ventanas)

        -- The remaining visible ventanas if the current one has been
        -- removed.
        notClosed =
            visRemove tp model.visVentanas

        -- visVentanas were the nearest element that shares a row with
        -- the element to be closed is set to visible.
        rowvis =
            case near of
                Just ( c, ( r, t ) ) ->
                    if c == column && r == row then
                        visInsert (c,(r,t)) notClosed

                    else
                        notClosed

                Nothing ->
                    notClosed

        -- Calculate new values for these variables.
        ( visible, focused, history ) =
            if wasVisible then
                if wasFocused then
                    case newHistory of
                        f :: h ->
                            -- The focus function adds the focused
                            -- item to the visVentanas dict, so we
                            -- only want to ensure that there is
                            -- something visible in the row.
                            ( rowvis, Just f, h )

                        [] ->
                            ( rowvis, near, [] )

                else
                    ( rowvis, model.focused, newHistory )

            else
                ( notClosed, model.focused, newHistory )

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

        -- When the tab's vista isn't global, meaning always
        -- available, and there are not multiple references to the
        -- vista, and the function was called with the closevista
        -- option set to true, remove the vista.
        vistas =
            if nonglobal && not multiref && closevista then
                Dict.remove vista model.vistas

            else
                model.vistas
    in
    case focused of
        Nothing ->
            -- The final window was closed.
            initData ( Dict.filter
                           (\v _ -> List.member v model.globalVistas )
                           model.vistas
                     )

        Just focusedTab ->
            focus focusedTab
                { model
                    | ventanas = ventanas
                    , visVentanas = visible
                    , vistas = vistas
                }


closeAll : String -> Model -> Model
closeAll vista model =
    getAllByVista vista model.ventanas
        |> List.foldl (closeTab False) model


{-| Assign a ventana to a new tab. The assumption is that the ventana
will be focused. This may change in the future.
-}
reassign : Path -> Path -> Model -> Model
reassign old new model =
    let
        ventanas =
            Dict.get old model.ventanas
                |> Maybe.map (\v -> Dict.insert new v model.ventanas)
                |> Maybe.withDefault model.ventanas

        model_ =
            focus new { model | ventanas = ventanas }
    in
    closeTab False old model_
                             


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


pathToString : Path -> String
pathToString ( c, ( r, t ) ) =
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


{-| Remove a tab from VisVentanas
-}
visRemove : Path -> VisVentanas -> VisVentanas
visRemove tp vis =
    let
        ( c, ( r, i ) ) =
            tp
    in
    Dict.filter (\( c1, r1 ) i1 -> not (c1 == c && r1 == r && i1 == i)) vis


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


{-| The first list is the history of tab focusing. The second is
actually open tabs. Remove items that have been closed from the
history.
-}
refreshHistory : List Path -> List Path -> List Path
refreshHistory history tabs =
    List.filter (\x -> List.member x tabs) history
        |> List.take 5


focus : Path -> Model -> Model
focus tp model =
    let
        tabs =
            Dict.keys model.ventanas

        requiresAdd previous_ =
            Just tp /= model.focused &&
                Just previous_ /= List.head model.focusHistory

        history =
            case model.focused of
                Nothing ->
                    refreshHistory model.focusHistory tabs

                Just previous ->
                    if requiresAdd previous then
                        refreshHistory
                        (previous :: model.focusHistory)
                        tabs

                    else
                        refreshHistory model.focusHistory tabs
    in
    { model
        | focused = Just tp
        , visVentanas = visInsert tp model.visVentanas
        , focusHistory = history
        , focusLock = Just tp
    }


focusWhenUnlocked : Path -> Model -> Model
focusWhenUnlocked tp model =
    case model.focusLock of
        Nothing ->
            focus tp model

        Just _ ->
            model


columnCount : List Path -> Int
columnCount tabs =
    let
        accCol : Path -> Set Int -> Set Int
        accCol (c, _) acc =
            Set.insert c acc
    in
    List.foldl accCol Set.empty tabs
        |> Set.size

           
multipleRows : List Path -> Bool
multipleRows tabs =
    let
        accRow : Path -> Dict Int (Set Int) -> Dict Int (Set Int)
        accRow (c, (r, _)) acc =
            case Dict.get c acc of
                Nothing ->
                    Dict.insert c (Set.insert r Set.empty) acc

                Just rows ->
                    Dict.insert c (Set.insert r rows) acc
    in
    List.foldl accRow Dict.empty tabs
        |> Dict.filter (\k v -> Set.size v > 1)
        |> Dict.isEmpty
        |> not
