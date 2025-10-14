module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport)
import Html
import Html.Attributes as Attr
import Html.Events as Event
import Json.Decode as D
import Json.Encode as E
import List.Extra as LE
import Maybe.Extra as ME
import Math.Vector3 as V3
import Task
import Dict exposing (Dict)
import Set exposing (Set)


-- A Ventana supplies the title and a referrence to a Vista, which is
-- an identifier for some viewable content. I use Spanish when there
-- are already commonly referred to object or concepts such as
-- "window" or "view".
type alias Ventana =
    { title : String
    , vista : String
    }


-- Viewable content.
type alias Vista =
    { project : String
    , kind : String
    , identifier : String
    , content : String
    }


-- The path to a tab, used for operations on tabs. 
type alias TabPath = (Int, (Int, Int))


-- All of the viewable content associated with a tab.
type alias Ventanas = Dict TabPath Ventana


-- Currently visible Ventanas.
type alias VisVentanas = Dict (Int, Int) Int


type alias Model =
    { counter : Int
    , ventanas : Ventanas
    , focused : Maybe TabPath
    , visVentanas : VisVentanas
    , windowHeight : Int
    }


type Direction = Left | Right | Up | Down
    

type Msg
    = NewTab Ventana
    | FocusTab TabPath
    | CloseTab TabPath
    | Move Direction


type alias Flags =
    { windowHeight : Int }


projects : List Ventana
projects = [ Ventana "first" "xxxx"
           , Ventana "second" "yyyy"
           , Ventana "third" "zzzz"
           ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { counter = 0
      , ventanas = Dict.empty
      , focused = Nothing
      , visVentanas = Dict.empty
      , windowHeight = flags.windowHeight
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewTab ventana ->
            if Dict.isEmpty model.ventanas then
                let
                    c = model.counter
                    tp = tabpath c c c
                    newmodel =
                        { model | counter = c + 1
                        , ventanas = Dict.singleton tp ventana
                        , visVentanas = visInsert tp Dict.empty
                        , focused = Just tp
                        }
                in
                ( newmodel, Cmd.none )

            else
                let
                    c = model.counter
                    -- The focused property is used to get the current column and
                    -- row. This is wrapped in a Maybe. It should not be Nothing.
                    -- In the unlikely case that it is, return the original model.
                    newmodel =
                        case model.focused of
                            Nothing ->
                                model

                            Just tp1 ->
                                let
                                    (column, (row, _)) = tp1
                                    tp2 = tabpath column row c
                                in
                                 { model | counter = c + 1
                                 , ventanas = Dict.insert tp2 ventana model.ventanas
                                 , visVentanas = visInsert tp2 model.visVentanas
                                 , focused = Just tp2
                                 }
                in
                ( newmodel, Cmd.none )

        FocusTab tp ->
            ( { model | focused = Just tp
              , visVentanas = visInsert tp model.visVentanas
              }
            , Cmd.none
            )

        CloseTab tp ->
            ( closeTab tp model, Cmd.none )

        Move Left ->
            -- If there is more than one tab, and one is focused, see
            -- if there is more than one in the row of the focused
            -- tab. If there is not, the row will close when the
            -- focused tab is removed. If there is, ensure that the
            -- nearest tab is made visible. If there is no left
            -- column, add a new column and row and create a tab that
            -- references the same ventana as the focused item. If
            -- there is not, create a new tab in the first row of the
            -- column, again referencing the original ventana. Delete
            -- the original focused item and focus the new tab.
            let
                vs = model.ventanas
                c = model.counter
                keys = Dict.keys vs
                cols = tcolumns keys
                newmodel =
                    if Dict.isEmpty vs || Dict.size vs == 1 then
                        model

                    else
                        case model.focused of
                            Just fp ->
                                let
                                    rows = trows (tcolumn fp) keys
                                in
                                -- Is the focused tab in the left-most column?
                                if Just (tcolumn fp) == LE.last cols then
                                    let
                                        newtp = (c, (c, ttab fp))
                                        moved = reassign fp newtp model
                                    in
                                    { moved | counter = c + 1 }

                                else
                                    let
                                        col = getNext (tcolumn fp) cols
                                        newrows = trows col keys
                                        new = List.head newrows
                                            |> Maybe.map
                                               (\r -> tabpath col r (ttab fp))
                                            |> Maybe.withDefault fp
                                        moved = reassign fp new model
                                    in
                                    reassign fp new model

                            Nothing ->
                                model

            in
            ( newmodel, Cmd.none )

        Move Down ->
            -- If there is more than one tab, and one is focused, see
            -- if there is more than one in the row of the focused
            -- tab. If there is not, the row will close when the
            -- focused tab is removed. If there is, ensure that the
            -- nearest tab is made visible. If there is no row below,
            -- add a new row and create a tab that references the same
            -- ventana as the focused item. If there is not, create a
            -- new tab in the row below, again referencing the
            -- original ventana. Delete the original focused item and
            -- focus the new tab.
            let
                vs = model.ventanas
                c = model.counter
                keys = Dict.keys vs
                cols = tcolumns keys
                newmodel =
                    if Dict.isEmpty vs || Dict.size vs == 1 then
                        model

                    else
                        case model.focused of
                            Just fp ->
                                let
                                    rows = trows (tcolumn fp) keys
                                in
                                -- Is the focused tab in the left-most column?
                                if Just (trow fp) == LE.last rows then
                                    let
                                        newtp = (tcolumn fp, (c, ttab fp))
                                        moved = reassign fp newtp model
                                    in
                                    { moved | counter = c + 1 }

                                else
                                    let
                                        row = getNext (trow fp) rows
                                        new = tabpath (tcolumn fp) row (ttab fp)
                                        moved = reassign fp new model
                                    in
                                    reassign fp new model

                            Nothing ->
                                model

            in
            ( newmodel, Cmd.none )
            
        Move _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

        
roleAttr : String -> Html.Attribute msg
roleAttr role =
    Attr.attribute "role" role


view : Model -> Html.Html Msg
view model =
    let
        tabtree = treeifyTabs <| Dict.keys model.ventanas
    in
    Html.main_ [ Attr.class "grid-with-side" ]
        [ Html.aside [ Attr.class "side" ]
              [ Html.nav []
                    [ Html.a [ Attr.href "#"
                             , Event.onClick <| Move Left
                             ] [ Html.text " Left " ]
                    , Html.a [ Attr.href "#"
                             , Event.onClick <| Move Down
                             ] [ Html.text " Down " ]
                    , Html.a [ Attr.href "#"
                             , Event.onClick <| Move Right
                             ] [ Html.text " Right " ]
                    , Html.a [ Attr.href "#"
                             , Event.onClick <| Move Up
                             ] [ Html.text " Up " ]
                    , Html.h2 [] [ Html.text "Projects" ]
                    , Html.ul [] (viewProjects model projects)
                    ]
              ] 
        , if Dict.isEmpty tabtree then
              Html.div
                  [ Attr.class "container-fluid" ]
                  [ Html.h1 [ Attr.class "title" ] [ Html.text "rrrrroar" ] ]

          else
              Html.div
                  [ Attr.class "container-fluid grid" ]
                  (Dict.map (viewColumn model) tabtree
                  |> Dict.toList
                  |> List.map Tuple.second)
        ]


viewColumn : Model -> Int -> Dict Int (Set Int) -> Html.Html Msg
viewColumn model col rows =
    Html.div [] (Dict.map (viewRow model col (Dict.size rows)) rows
                |> Dict.toList
                |> List.map Tuple.second)


viewRow : Model -> Int -> Int -> Int -> Set Int -> Html.Html Msg
viewRow model col rowcount row tabs =
    Html.div [ Attr.height (model.windowHeight // rowcount) ]
        [ Html.nav [ roleAttr "group"
                   , Attr.class "tabs-header"
                   ]
              (Set.toList tabs
              |> List.map (\t -> viewTabHeader model (tabpath col row t)))
        , Html.div []
            (Set.toList tabs
            |> List.map (\t -> viewTab model (tabpath col row t)))
        ]


viewTabHeader : Model -> TabPath -> Html.Html Msg
viewTabHeader model tp =
    Html.button
        [ Event.onClick (FocusTab tp)
        , Attr.class (if Just tp == model.focused then
                          "focused"
                          
                      else
                          if visMember tp model.visVentanas then
                              "secondary"

                          else
                              "secondary outline"
                     )
        ] [ Dict.get tp model.ventanas
          |> Maybe.withDefault { title = "Error"
                               , vista = "Vista not found"
                               }
          |> .title
          |> Html.text
          ]


viewTab : Model -> TabPath -> Html.Html Msg
viewTab model tp =
    Html.div [ Attr.classList
                   [ ( "focused", Just tp == model.focused)
                   , ( "hidden", not (visMember tp model.visVentanas) )
                   ]
             ]
        [ Html.div [ Attr.class "tab-inner-header" ]
              [ Html.a [ Attr.href "#"
                       ,Event.onClick (CloseTab tp)
                       ]
                    [ Html.text "Close" ]
              ]
        , Html.div [] [ Dict.get tp model.ventanas
                      |> Maybe.withDefault { title = "Error"
                                           , vista = "Vista not found"
                                           }
                      |> .vista
                      |> Html.text
                      ]
        ]


viewProjects : Model -> List Ventana -> List (Html.Html Msg)
viewProjects model ps =
    List.map (\w -> Html.li []
                  [ Html.a
                        [ Attr.href ("#" ++ w.vista)
                        , Event.onClick (NewTab w)
                        ]
                        [ Html.text w.title ] ]
             ) ps
    

-- Return the TabPaths for the tabs in the same row.
sharesRow : TabPath -> Model -> List TabPath
sharesRow tp model =
    let
        matchrow tp2 = trow tp == trow tp2 && tp /= tp2
    in
    List.filter matchrow (Dict.keys model.ventanas)


-- This uses vector distance to find a new focused item. It is called
-- in instances such as the closing of a tab. The notion of distance
-- isn't really very relevant. The idea is that some tab should become
-- open and focused if there is another tab in the same row. If there
-- is not, then some tab that is open in some other row should be
-- focused. Using distance may result in an intuitive behavior or not.
nearest : TabPath -> Model -> Maybe TabPath
nearest tp model =
    let
        toV3 (column, (row, tab)) =
            V3.vec3 (toFloat column) (toFloat row) (toFloat tab)
        vp = toV3 tp
        nearest_ tps =
            List.map (\t -> (V3.distance (toV3 t) vp, t)) tps
                |> List.minimum
                |> Maybe.map Tuple.second
                |> (\t -> if t == (Just tp) then Nothing else t)
    in
    case sharesRow tp model of
        [] ->
            nearest_ (visRemove tp model.visVentanas |> visToList)

        tps ->
            nearest_ tps


-- This will close a tab and set the nearest tab to focused.
closeTab : TabPath -> Model -> Model
closeTab tp model =
    let
        notClosed = visRemove tp model.visVentanas
        focused = nearest tp model
        visVentanas = ME.unwrap notClosed
                      (\f -> visInsert f notClosed) focused
    in
    { model | ventanas = Dict.remove tp model.ventanas
    , focused = focused
    , visVentanas = visVentanas
    }


-- Assign a ventana to a new tab.
reassign : TabPath -> TabPath -> Model -> Model
reassign old new model =
    let
        closed = closeTab old model
        ventanas =
            Dict.get old model.ventanas
                |> Maybe.map (\v -> Dict.insert new v closed.ventanas)
                |> Maybe.withDefault closed.ventanas
        visVentanas = visInsert new closed.visVentanas
    in
    { closed | ventanas = ventanas
    , visVentanas = visVentanas
    , focused = Just new
    }


-- Attempts to find the integer after the current one in a list and
-- returns the current integer on failure.
getNext : Int -> List Int -> Int
getNext curr others =
    List.partition (\x -> x <= curr) others
        |> Tuple.second
        |> List.head
        |> Maybe.withDefault curr
        

-- Create a tree structure from the flat path listing of TabPaths to
-- be used by the view function.
treeifyTabs : List TabPath -> Dict Int (Dict Int (Set Int))
treeifyTabs tps =
    List.foldl
        (\tp tr ->
             case Dict.get (tcolumn tp) tr of
                 Nothing ->
                     let
                         newtabs = Set.singleton (ttab tp)
                         newrows = Dict.singleton (trow tp) newtabs
                     in
                     Dict.insert (tcolumn tp) newrows tr

                 Just rows ->
                     case Dict.get (trow tp) rows of
                         Nothing ->
                             let
                                 newtabs = Set.singleton (ttab tp)
                                 newrows = Dict.insert (trow tp) newtabs rows
                             in
                             Dict.insert (tcolumn tp) newrows tr

                         Just tabs ->
                             if not (Set.member (ttab tp) tabs) then
                                 let
                                     newtabs = Set.insert (ttab tp) tabs
                                     newrows = Dict.insert (trow tp) newtabs rows
                                 in
                                 Dict.insert (tcolumn tp) newrows tr

                             else
                                 -- Unexpected
                                 tr
        ) Dict.empty tps

        
-- TabPath helper functions are mostly used to help document the
-- intention of working with the integer tuple.
tabpath : Int -> Int -> Int -> TabPath
tabpath c r t = (c, (r, t))
                

tcolumn : TabPath -> Int
tcolumn tp = Tuple.first tp


trow : TabPath -> Int
trow tp = tp |> Tuple.second |> Tuple.first


ttab : TabPath -> Int
ttab tp = tp |> Tuple.second |> Tuple.second


-- All columns in order
tcolumns : List TabPath -> List Int
tcolumns tps =
    List.map tcolumn tps
        |> LE.unique
        |> List.sort


-- Rows for a column in order
trows : Int -> List TabPath -> List Int
trows column tps =
    List.filter (\tp -> tcolumn tp == column) tps
        |> List.map trow
        |> LE.unique
        |> List.sort


-- Insert a tab into VisVentanas
visInsert : TabPath -> VisVentanas -> VisVentanas
visInsert tp vv =
    Dict.insert (tcolumn tp, trow tp) (ttab tp) vv


-- Insert a tab into VisVentanas
visRemove : TabPath -> VisVentanas -> VisVentanas
visRemove tp vv =
    Dict.remove (tcolumn tp, trow tp) vv
        

-- Assert whether a tab is visible
isVisible : TabPath -> VisVentanas -> Bool
isVisible (col, (row, tab)) vv =
    case Dict.get (col, row) vv of
        Just t ->
            t == tab

        Nothing ->
            False


visToList : VisVentanas -> List TabPath
visToList vv =
    Dict.toList vv
        |> List.map (\((c, r), t) -> (c, (r, t)))


visMember : TabPath -> VisVentanas -> Bool
visMember (col, (row, tab)) vv =
    case Dict.get (col, row) vv of
        Nothing ->
            False

        Just t ->
            (col, (row, tab)) == (col, (row, t))
