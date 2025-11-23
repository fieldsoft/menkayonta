port module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport)
import Dict exposing (Dict)
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as FParse
import FormToolkit.Value as Value
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import Json.Decode as D
import Json.Encode as E
import List.Extra as LE
import Math.Vector3 as V3
import Maybe.Extra as ME
import Result
import Set exposing (Set)
import Task


port setWindowTitle : String -> Cmd msg


port requestProjectIndex : String -> Cmd msg


port requestGlobalConfig : () -> Cmd msg


port receivedGlobalConfig : (E.Value -> msg) -> Sub msg


port receivedProjectIndex : (E.Value -> msg) -> Sub msg


port newProject : (String -> msg) -> Sub msg


port createProject : E.Value -> Cmd msg


{-| A Ventana supplies the title and a referrence to a Vista, which is
an identifier for some viewable content. I use Spanish when there are
already commonly referred to object or concepts such as "window" or
"view".
-}
type alias Ventana =
    { title : String
    , vista : String
    }


{-| All of the viewable content associated with a tab.
-}
type alias Ventanas =
    Dict TabPath Ventana


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
type alias TabPath =
    ( Int, ( Int, Int ) )


{-| Currently visible Ventanas.
-}
type alias VisVentanas =
    Dict ( Int, Int ) Int


type alias GlobalConfig =
    { projects : List ProjectInfo }


type alias ProjectInfo =
    { title : String
    , identifier : String
    }


type alias Error =
    { message : String }


type alias Model =
    { gconfig : Maybe GlobalConfig
    , counter : Int
    , ventanas : Ventanas
    , focused : Maybe TabPath
    , visVentanas : VisVentanas
    , vistas : Vistas
    , windowHeight : Int
    , error : Maybe Error
    , projectFields : Field ProjectField
    , projectSubmitted : Bool
    }


type Direction
    = Left
    | Right
    | Up
    | Down


type ProjectField
    = ProjectIdentifier
    | ProjectTitle


type alias FieldKind =
    ProjectField


type Msg
    = NewTab Ventana
    | FocusTab TabPath
    | CloseTab TabPath
    | Move Direction
    | SetWindowTitle String
    | ReceivedGlobalConfig E.Value
    | ReceivedProjectIndex E.Value
    | RequestProjectIndex String
    | NewProject String
    | FormChange (Field.Msg FieldKind)
    | FormSubmit TabPath


type alias Flags =
    { windowHeight : Int }


type Content
    = DemoContent Demo
    | DemoListContent (List Demo)
    | TranslationContent Translation
    | TranslationsContent (List Translation)
    | ProjectInfoContent ProjectInfo
    | ErrorContent Error


type alias Translation =
    { source : String
    , translation : String
    }


type alias Demo =
    { source : String
    , translation : String
    , parse : String
    , gloss : String
    }


vistas : Dict String Vista
vistas =
    [ ( "new-project"
      , { project = "global"
        , kind = "new-project"
        , identifier = "new-project"
        , content = ProjectInfoContent (ProjectInfo "" "")
        }
      )
    ]
        |> Dict.fromList


projectFields : String -> Field ProjectField
projectFields ident =
    Field.group []
        [ Field.text
            [ Field.label "Identifier"
            , Field.required True
            , Field.disabled True
            , Field.identifier ProjectIdentifier
            , Field.name "identifier"
            , Field.value (Value.string ident)
            ]
        , Field.text
            [ Field.label "Title"
            , Field.required True
            , Field.identifier ProjectTitle
            , Field.name "title"
            ]
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
    ( { gconfig = Nothing
      , counter = 0
      , ventanas = Dict.empty
      , focused = Nothing
      , visVentanas = Dict.empty
      , vistas = vistas
      , windowHeight = flags.windowHeight
      , error = Nothing
      , projectFields = projectFields ""
      , projectSubmitted = False
      }
    , requestGlobalConfig ()
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewTab ventana ->
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
                                    | counter = c + 1
                                    , ventanas = Dict.insert tp2 ventana model.ventanas
                                    , visVentanas = visInsert tp2 model.visVentanas
                                    , focused = Just tp2
                                }
                in
                ( newmodel, Cmd.none )

        FocusTab tp ->
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
            , setWindowTitle ("Menkayonta: " ++ title)
            )

        CloseTab tp ->
            ( closeTab tp model, Cmd.none )

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

        SetWindowTitle title ->
            ( model, setWindowTitle title )

        ReceivedGlobalConfig gc ->
            case D.decodeValue globalConfigDecoder gc of
                Err err ->
                    ( { model | error = Just (Error (D.errorToString err)) }
                    , Cmd.none
                    )

                Ok gconfig ->
                    ( { model | gconfig = Just gconfig }, Cmd.none )

        RequestProjectIndex id ->
            ( model, requestProjectIndex id )

        ReceivedProjectIndex pi ->
            case D.decodeValue vistaDecoder pi of
                Err err ->
                    ( { model | error = Just (Error (D.errorToString err)) }
                    , Cmd.none
                    )

                Ok v ->
                    case getProjectTitle v.project model of
                        Nothing ->
                            ( { model
                                | error = Just (Error "No such project")
                              }
                            , Cmd.none
                            )

                        Just title ->
                            let
                                newmodel =
                                    { model
                                        | vistas =
                                            Dict.insert
                                                v.identifier
                                                v
                                                model.vistas
                                    }
                            in
                            case getByVista v.identifier model.ventanas of
                                Nothing ->
                                    update
                                        (NewTab <| Ventana title v.identifier)
                                        newmodel

                                Just tp ->
                                    update (FocusTab tp) newmodel

        -- Open or focus the New Project form.
        NewProject ident ->
            let
                newmodel =
                    { model
                        | projectFields = projectFields ident
                        , projectSubmitted = False
                    }
            in
            case getByVista "new-project" model.ventanas of
                Nothing ->
                    update (NewTab <| Ventana "New Project" "new-project") newmodel

                Just tp ->
                    update (FocusTab tp) newmodel

        FormChange inputMsg ->
            let
                ( formFields, _ ) =
                    FParse.parseUpdate projectParser inputMsg model.projectFields
            in
            ( { model
                | projectFields = formFields
                , projectSubmitted = False
              }
            , Cmd.none
            )

        FormSubmit tp ->
            if model.projectSubmitted /= True then
                case FParse.parseValidate FParse.json model.projectFields of
                    ( formFields, Ok jsonValue ) ->
                        ( { model
                            | projectFields = projectFields ""
                            , projectSubmitted = True
                          }
                        , createProject jsonValue
                        )

                    ( formFields, Err _ ) ->
                        ( { model
                            | projectFields = formFields
                            , projectSubmitted = False
                          }
                        , Cmd.none
                        )

            else
                update (CloseTab tp) model


{-| insertTabPath, newTabPath, and createNecessary are all helpers for
Move Direction. Each provides Direction specific code for some
aspect of the Move operation. This is for the case when movement
places the focused tab in a preexisting row with tabs.
-}
insertTabPath : Direction -> TabPath -> ( List Int, List Int ) -> List TabPath -> TabPath
insertTabPath dir tp ( cols, rows ) keys =
    case dir of
        Left ->
            let
                col =
                    getNext (tcolumn tp) cols

                newrows =
                    trows col keys
            in
            List.head newrows
                |> Maybe.map (\r -> tabpath col r (ttab tp))
                |> Maybe.withDefault tp

        Right ->
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


{-| Use the counter (c) to provide new TabPaths that will be rendered
below, above, to the left or right of the focused tab.
-}
newTabPath : Direction -> TabPath -> Int -> TabPath
newTabPath dir tp c =
    case dir of
        Left ->
            tabpath c c (ttab tp)

        Right ->
            tabpath (negate c) c (ttab tp)

        Down ->
            tabpath (tcolumn tp) c (ttab tp)

        Up ->
            tabpath (tcolumn tp) (negate c) (ttab tp)


{-| Essentially check the heads and tails of the relevant lists of
columns or rows to determine if the current item is on the border
of a column or row structure.
-}
createNecessary : Direction -> TabPath -> ( List Int, List Int ) -> Bool
createNecessary dir tp ( cols, rows ) =
    case dir of
        Left ->
            Just (tcolumn tp) == LE.last cols

        Right ->
            Just (tcolumn tp) == List.head cols

        Down ->
            Just (trow tp) == LE.last rows

        Up ->
            Just (trow tp) == List.head rows


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receivedGlobalConfig ReceivedGlobalConfig
        , receivedProjectIndex ReceivedProjectIndex
        , newProject NewProject
        ]


{-| An HTML attibute that isn't included in the standard Elm library.
-}
roleAttr : String -> Html.Attribute msg
roleAttr role =
    Attr.attribute "role" role


view : Model -> Html.Html Msg
view model =
    let
        tabtree =
            treeifyTabs <| Dict.keys model.ventanas
    in
    Html.main_ [ Attr.class "grid-with-side" ]
        [ Html.aside [ Attr.class "side" ]
            [ Html.nav []
                [ Html.a
                    [ Attr.href "#"
                    , Event.onClick <| Move Left
                    ]
                    [ Html.text " Left " ]
                , Html.a
                    [ Attr.href "#"
                    , Event.onClick <| Move Down
                    ]
                    [ Html.text " Down " ]
                , Html.a
                    [ Attr.href "#"
                    , Event.onClick <| Move Right
                    ]
                    [ Html.text " Right " ]
                , Html.a
                    [ Attr.href "#"
                    , Event.onClick <| Move Up
                    ]
                    [ Html.text " Up " ]
                , Html.h2 [] [ Html.text "Projects" ]
                , viewProjects model
                ]
            ]
        , if Dict.isEmpty tabtree then
            Html.div
                [ Attr.class "container-fluid" ]
                [ Html.h1 [ Attr.class "title" ] [ Html.text "rrrrroar" ] ]

          else
            Html.div
                [ Attr.class "cotainer-fluid grid"
                , Attr.style "height" (String.fromInt model.windowHeight ++ "px")
                ]
                (Dict.map (viewColumn model) tabtree
                    |> Dict.toList
                    |> List.map Tuple.second
                )
        ]


viewColumn : Model -> Int -> Dict Int (Set Int) -> Html.Html Msg
viewColumn model col rows =
    Html.div [ Attr.class "wrapper" ]
        (Dict.map (viewRow model col (Dict.size rows)) rows
            |> Dict.toList
            |> List.map Tuple.second
        )


viewRow : Model -> Int -> Int -> Int -> Set Int -> Html.Html Msg
viewRow model col rowcount row tabs =
    Html.div []
        [ Html.nav
            [ roleAttr "group"
            , Attr.class "tabs-header"
            ]
            (Set.toList tabs
                |> List.map (\t -> viewTabHeader model (tabpath col row t))
            )
        , Html.div []
            (Set.toList tabs
                |> List.map (\t -> viewTab model (tabpath col row t))
            )
        ]


viewTabHeader : Model -> TabPath -> Html.Html Msg
viewTabHeader model tp =
    Html.button
        [ Event.onClick (FocusTab tp)
        , Attr.class
            (if Just tp == model.focused then
                "focused"

             else if visMember tp model.visVentanas then
                "secondary"

             else
                "secondary outline"
            )
        ]
        [ Dict.get tp model.ventanas
            |> Maybe.withDefault
                { title = "Error"
                , vista = "Vista not found"
                }
            |> .title
            |> Html.text
        ]


viewTab : Model -> TabPath -> Html.Html Msg
viewTab model tp =
    Html.div
        [ Attr.classList
            [ ( "focused", Just tp == model.focused )
            , ( "hidden", not (visMember tp model.visVentanas) )
            ]
        ]
        [ Html.div [ Attr.class "tab-inner-header" ]
            [ Html.a
                [ Attr.href "#"
                , Event.onClick (CloseTab tp)
                ]
                [ Html.text "Close" ]
            ]
        , Html.div []
            [ Dict.get tp model.ventanas
                |> Maybe.andThen (\v -> Dict.get v.vista model.vistas)
                |> Maybe.map (viewVista model tp)
                |> Maybe.withDefault (Html.text "Failed!")
            ]
        ]


viewProjects : Model -> Html.Html Msg
viewProjects model =
    case model.gconfig of
        Nothing ->
            Html.text "Waiting for configuration to load"

        Just gconfig ->
            Html.ul []
                (List.map
                    (\p ->
                        Html.li []
                            [ Html.a
                                [ Attr.href ("#" ++ p.identifier)
                                , Event.onClick (RequestProjectIndex p.identifier)
                                ]
                                [ Html.text p.title ]
                            ]
                    )
                    gconfig.projects
                )


viewVista : Model -> TabPath -> Vista -> Html Msg
viewVista model tp vista =
    case vista.content of
        DemoContent dc ->
            viewDemo model dc

        DemoListContent dcs ->
            Html.table [] (List.map (viewDemo model) dcs)

        ProjectInfoContent pi ->
            Html.form [ Event.onSubmit (FormSubmit tp) ]
                [ Field.toHtml FormChange model.projectFields
                , Html.button [ Event.onClick (FormSubmit tp) ]
                    [ Html.text "Save" ]
                ]

        TranslationContent trn ->
            viewTranslation model trn

        TranslationsContent trns ->
            Html.table [] (List.map (viewTranslation model) trns)

        ErrorContent err ->
            viewError model err


viewError : Model -> Error -> Html Msg
viewError _ err =
    Html.text err.message


viewDemo : Model -> Demo -> Html.Html Msg
viewDemo model dc =
    Html.tr []
        [ Html.td [] [ Html.text dc.source ]
        , Html.td [] [ Html.text dc.parse ]
        , Html.td [] [ Html.text dc.gloss ]
        , Html.td [] [ Html.text dc.translation ]
        ]


viewTranslation : Model -> Translation -> Html.Html Msg
viewTranslation model dc =
    Html.tr []
        [ Html.td [] [ Html.text dc.source ]
        , Html.td [] [ Html.text dc.translation ]
        ]


{-| Return the TabPaths for the tabs in the same row.
-}
sharesRow : TabPath -> Model -> List TabPath
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
nearest : TabPath -> Model -> Maybe TabPath
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
closeTab : TabPath -> Model -> Model
closeTab tp model =
    let
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
    }


{-| Assign a ventana to a new tab.
-}
reassign : TabPath -> TabPath -> Model -> Model
reassign old new model =
    let
        closed =
            closeTab old model

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


{-| Create a tree structure from the flat path listing of TabPaths to
be used by the view function.
-}
treeifyTabs : List TabPath -> Dict Int (Dict Int (Set Int))
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



{- TabPath helper functions are mostly used to help document the
   intention of working with the integer tuple.
-}


tabpath : Int -> Int -> Int -> TabPath
tabpath c r t =
    ( c, ( r, t ) )


tcolumn : TabPath -> Int
tcolumn tp =
    Tuple.first tp


trow : TabPath -> Int
trow tp =
    tp |> Tuple.second |> Tuple.first


ttab : TabPath -> Int
ttab tp =
    tp |> Tuple.second |> Tuple.second


{-| All columns in order
-}
tcolumns : List TabPath -> List Int
tcolumns tps =
    List.map tcolumn tps
        |> LE.unique
        |> List.sort


{-| Rows for a column in order
-}
trows : Int -> List TabPath -> List Int
trows column tps =
    List.filter (\tp -> tcolumn tp == column) tps
        |> List.map trow
        |> LE.unique
        |> List.sort


{-| Insert a tab into VisVentanas
-}
visInsert : TabPath -> VisVentanas -> VisVentanas
visInsert tp vv =
    Dict.insert ( tcolumn tp, trow tp ) (ttab tp) vv


{-| Insert a tab into VisVentanas
-}
visRemove : TabPath -> VisVentanas -> VisVentanas
visRemove tp vv =
    Dict.remove ( tcolumn tp, trow tp ) vv


{-| True when a tab is visible
-}
isVisible : TabPath -> VisVentanas -> Bool
isVisible ( col, ( row, tab ) ) vv =
    case Dict.get ( col, row ) vv of
        Just t ->
            t == tab

        Nothing ->
            False


visToList : VisVentanas -> List TabPath
visToList vv =
    Dict.toList vv
        |> List.map (\( ( c, r ), t ) -> ( c, ( r, t ) ))


visMember : TabPath -> VisVentanas -> Bool
visMember ( col, ( row, tab ) ) vv =
    case Dict.get ( col, row ) vv of
        Nothing ->
            False

        Just t ->
            ( col, ( row, tab ) ) == ( col, ( row, t ) )


vistaDecoder : D.Decoder Vista
vistaDecoder =
    D.map4 Vista
        (D.field "project" D.string)
        (D.field "kind" D.string)
        (D.field "identifier" D.string)
        (D.field "kind" D.string |> D.andThen contentDecoder)


contentDecoder : String -> D.Decoder Content
contentDecoder kind =
    case kind of
        "all-translations" ->
            D.map TranslationsContent
                (D.field "content" translationsDecoder)

        "new-project" ->
            D.map ProjectInfoContent
                (D.field "content" projectInfoDecoder)

        _ ->
            D.fail ("Unsupported content kind " ++ kind)


globalConfigDecoder : D.Decoder GlobalConfig
globalConfigDecoder =
    D.map GlobalConfig
        (D.field "projects" (D.list projectInfoDecoder))


projectInfoDecoder =
    D.map2 ProjectInfo
        (D.field "title" D.string)
        (D.field "identifier" D.string)


translationsDecoder : D.Decoder (List Translation)
translationsDecoder =
    D.list translationDecoder


translationDecoder : D.Decoder Translation
translationDecoder =
    D.map2 Translation
        (D.field "source" D.string)
        (D.field "translation" D.string)


projectParser =
    FParse.map2 ProjectInfo
        (FParse.field ProjectIdentifier FParse.string)
        (FParse.field ProjectTitle FParse.string)


getByVista : String -> Dict TabPath Ventana -> Maybe TabPath
getByVista vista ventanas =
    let
        keys =
            List.filter (\( k, v ) -> v.vista == vista) (Dict.toList ventanas)
    in
    case keys of
        [] ->
            Nothing

        ( key, _ ) :: _ ->
            Just key


getProjectTitle : String -> Model -> Maybe String
getProjectTitle projid model =
    model.gconfig
        |> Maybe.map .projects
        |> Maybe.andThen (LE.find (\x -> x.identifier == projid))
        |> Maybe.map .title
