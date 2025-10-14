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
type alias VisVentanas = Set TabPath


type alias Model =
    { counter : Int
    , ventanas : Ventanas
    , focused : Maybe TabPath
    , visVentanas : VisVentanas
    , windowHeight : Int
    }


type Msg
    = NewTab Ventana
    | FocusTab TabPath
    | CloseTab TabPath


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
      , visVentanas = Set.empty
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
                        , visVentanas = Set.singleton tp
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
                                 , visVentanas = Set.remove tp1 model.visVentanas
                                                  |> Set.insert tp2
                                 , focused = Just tp2
                                 }
                in
                ( newmodel, Cmd.none )

        FocusTab tp ->
            -- When a new tab is focused in the same row as the
            -- previously focused tab, the previously focused tab must
            -- become unopened.
            let
                unopen =
                    case model.focused of
                        Nothing ->
                            Nothing

                        Just fp ->
                            if tcolumn tp == tcolumn fp && trow tp == trow fp then
                                Just fp

                            else
                                Nothing
                newmodel =
                    { model | focused = Just tp
                    , visVentanas = Set.insert tp model.visVentanas
                    |> (\ovs -> ME.unwrap ovs (\t -> Set.remove t ovs) unopen)
                    }
            in
            ( newmodel, Cmd.none )

        CloseTab tp ->
            let
                notClosed = Set.remove tp model.visVentanas
                focused = recalcFocus tp model
                visVentanas = ME.unwrap notClosed
                              (\f -> Set.insert f notClosed) focused
                newmodel =
                    { model | ventanas = Dict.remove tp model.ventanas
                    , focused = focused
                    , visVentanas = visVentanas
                    }
            in
            ( newmodel, Cmd.none )


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
                    [ Html.h2 [] [ Html.text "Projects" ]
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
        , Attr.classList [ ( "focused", Just tp == model.focused )
                         , ( "secondary outline"
                           , not (Set.member tp model.visVentanas)
                           )
                         ]
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
                   , ( "hidden", not (Set.member tp model.visVentanas) )
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
recalcFocus : TabPath -> Model -> Maybe TabPath
recalcFocus tp model =
    let
        toV3 (column, (row, tab)) =
            V3.vec3 (toFloat column) (toFloat row) (toFloat tab)
        vp = toV3 tp
        nearest tps =
            List.map (\t -> (V3.distance (toV3 t) vp, t)) tps
                |> List.minimum
                |> Maybe.map Tuple.second
                |> (\t -> if t == (Just tp) then Nothing else t)
    in
    case sharesRow tp model of
        [] ->
            nearest (Set.remove tp model.visVentanas |> Set.toList)

        tps ->
            nearest tps
                

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
