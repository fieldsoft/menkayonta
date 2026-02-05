port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Config exposing (GlobalConfig, GlobalSettings, ProjectInfo)
import Content exposing (Content(..))
import Dict exposing (Dict)
import Email exposing (Email)
import Form.Global
import Form.Interlinear
import Form.Project
import Form.Importer
import Form.Shared exposing (blankSelect)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import Iso8601
import Json.Decode as D
import Json.Encode as E
import Json.Encode.Extra as EE
import List.Extra as LE
import Math.Vector3 as V3
import Maybe.Extra as ME
import Menkayonta as M exposing (Interlinear)
import Random
import Result
import Set exposing (Set)
import Tab
    exposing
        ( Direction(..)
        , Ventana
        , VentanaParams
        , Ventanas
        , VisVentanas
        , Vista
        , Vistas
        , defVParams
        )
import Task
import Time
import UUID
import Url


type alias Model =
    { gconfig : Maybe GlobalConfig
    , me : Maybe M.Person
    , counter : Int
    , ventanas : Ventanas
    , focused : Maybe Tab.Path
    , visVentanas : VisVentanas
    , vistas : Vistas
    , error : String
    , loading : Set String
    , seeds : UUID.Seeds
    }


type Msg
    = ChangeLengthParam Tab.Path String
    | ChangeSearchParam Tab.Path String
    | GF Form.Global.Msg
    | IM Form.Importer.Msg
    | ITE UUID.UUID Form.Interlinear.Msg
    | EditImporter String
    | MultiMsg (List Msg)
    | None
    | PR UUID.UUID Form.Project.Msg
    | ReceivedAllDoc E.Value
    | ReceivedGlobalConfig E.Value
    | ReceivedInterlinearIndex E.Value
    | RequestAllDocId String String
    | RequestDocId String String
    | RequestInterlinearIndex UUID.UUID
    | SetWindowTitle String
    | ShowGlobalSettings E.Value
    | NewInterlinear UUID.UUID
    | EditInterlinear UUID.UUID M.Interlinear
    | NewProject
    | EditProject Form.Project.Model
    | Stamp (Time.Posix -> Envelope) (E.Value -> Cmd Msg) Time.Posix
    | Tab Tab.Msg


{-| Inject a message into `Cmd`
-}
sendMsg : Msg -> Cmd Msg
sendMsg msg =
    Task.succeed msg
        |> Task.perform identity


type alias Envelope =
    { command : String
    , project : String
    , address : String
    , content : E.Value
    }


envelopeDecoder : D.Decoder Envelope
envelopeDecoder =
    D.map4 Envelope
        (D.field "command" D.string)
        (D.field "project" D.string)
        (D.field "address" D.string)
        (D.field "content" D.value)


envelopeEncoder : Envelope -> E.Value
envelopeEncoder env =
    E.object
        [ ( "command", E.string env.command )
        , ( "project", E.string env.project )
        , ( "address", E.string env.address )
        , ( "content", env.content )
        ]


globalSettingsVista : Vista
globalSettingsVista =
    { project = "global"
    , kind = "global-settings"
    , identifier = "global-settings"
    , content = Content.GF Form.Global.initData
    }


importOptionsVista : Vista
importOptionsVista =
    { project = "global"
    , kind = "import-options"
    , identifier = "import-options"
    , content = Content.IM Form.Importer.initData
    }

{-| These are not specific to any project and are kept around, even
when not in use.
-}
globalVistas : Dict String Vista
globalVistas =
    [ ( "import-options"
      , importOptionsVista
      )
    , ( "global-settings"
      , globalSettingsVista
      )
    ]
        |> Dict.fromList


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Flags =
    { seed1 : Int
    , seed2 : Int
    , seed3 : Int
    , seed4 : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        seeds =
            { seed1 = Random.initialSeed flags.seed1
            , seed2 = Random.initialSeed flags.seed2
            , seed3 = Random.initialSeed flags.seed3
            , seed4 = Random.initialSeed flags.seed4
            }

        ( gmodel, gmsg ) =
            Form.Global.init E.null

        gv =
            Dict.get "global-settings" globalVistas
                |> Maybe.map (\gs -> { gs | content = Content.GF gmodel })
                |> Maybe.map
                    (\gs -> Dict.insert "global-settings" gs globalVistas)
                |> Maybe.withDefault globalVistas
    in
    ( { gconfig = Nothing
      , me = Nothing
      , counter = 0
      , ventanas = Dict.empty
      , focused = Nothing
      , visVentanas = Dict.empty
      , vistas = gv
      , error = ""
      , loading = Set.empty
      , seeds = seeds
      }
    , Cmd.batch
        [ requestGlobalConfig ()
        , Cmd.map GF gmsg
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- A message for doing nothing
        None ->
            ( model, Cmd.none )

        -- A message of many messages
        MultiMsg msgs ->
            ( model, msgs |> List.map sendMsg |> Cmd.batch )

        Stamp envelopePart cmd time ->
            let
                envelope =
                    envelopePart time |> envelopeEncoder
            in
            ( model, cmd envelope )

        ITE id subMsg ->
            let
                id_ =
                    "FORM::" ++ UUID.toString id

                maybeVista =
                    Dict.get id_ model.vistas

                maybeTab =
                    getByVista id_ model.ventanas
            in
            case ( maybeVista, maybeTab ) of
                ( Just vista_, Just tp ) ->
                    let
                        oldModel =
                            case vista_.content of
                                Content.ITE model_ ->
                                    model_

                                _ ->
                                    Form.Interlinear.initData id

                        ( subModel, subCmd ) =
                            Form.Interlinear.update subMsg oldModel

                        vista =
                            { vista_ | content = Content.ITE subModel }

                        vistas =
                            Dict.insert id_ vista model.vistas

                        nmodel =
                            { model | vistas = vistas }
                    in
                    -- For most messages, a pass-through is
                    -- sufficient. In some cases, there are side
                    -- effects and UI events that need to be triggered
                    -- outside the submodule.
                    case subMsg of
                        Form.Interlinear.Cancel ->
                            ( nmodel
                            , Cmd.batch
                                [ sendMsg (Tab Tab.Close)
                                , Cmd.map (ITE id) subCmd
                                ]
                            )

                        Form.Interlinear.Save ->
                            let
                                envelopePart =
                                    prepInterlinearSave
                                        subModel
                                        vista.project
                                        model.me
                            in
                            ( nmodel
                            , Cmd.batch
                                [ sendMsg (Tab Tab.Close)
                                , Task.perform
                                    (Stamp envelopePart send)
                                    Time.now
                                , Cmd.map (ITE id) subCmd
                                ]
                            )

                        _ ->
                            ( nmodel, Cmd.map (ITE id) subCmd )

                _ ->
                    -- Something is wrong. Ignore the message.
                    ( model, Cmd.none )

        IM subMsg ->
            let
                oldModel =
                    Dict.get "import-options" model.vistas
                        |> Maybe.map .content
                        |> (\c ->
                                case c of
                                    Just (Content.IM model_) ->
                                        model_

                                    _ ->
                                        -- this has no filepath, so it
                                        -- will do nothing.
                                        Form.Importer.initData
                           )

                ( subModel, subCmd ) =
                    Form.Importer.update subMsg oldModel

                vista =
                    { importOptionsVista | content = Content.IM subModel }

                vistas =
                    Dict.insert "import-options" vista model.vistas

                nmodel =
                    { model | vistas = vistas }
            in
            -- For most messages, a pass-through is sufficient. In
            -- some cases, there are side effects and UI events that
            -- need to be triggered outside the submodule.
            case subMsg of
                Form.Importer.Cancel ->
                    ( nmodel
                    , Cmd.batch
                        [ sendMsg (Tab Tab.Close)
                        , Cmd.map IM subCmd
                        ]
                    )

                Form.Importer.Import ->
                    let
                        jsonValue =
                            E.object
                                [ ( "filepath", E.string subModel.filepath )
                                , ( "kind", E.string subModel.kind.value )
                                , ( "project", E.string subModel.project.value )
                                ]
                    in
                    ( nmodel
                    , Cmd.batch
                        [ sendMsg (Tab Tab.Close)
                        , importFile jsonValue
                        , Cmd.map IM subCmd
                        ]
                    )

                _ ->
                    ( nmodel, Cmd.map IM subCmd )

        GF subMsg ->
            let
                oldModel =
                    Dict.get "global-settings" model.vistas
                        |> Maybe.map .content
                        |> (\c ->
                                case c of
                                    Just (Content.GF model_) ->
                                        model_

                                    _ ->
                                        Form.Global.initData
                           )

                ( subModel, subCmd ) =
                    Form.Global.update subMsg oldModel

                vista =
                    { globalSettingsVista | content = Content.GF subModel }

                vistas =
                    Dict.insert "global-settings" vista model.vistas

                nmodel =
                    { model | vistas = vistas }
            in
            -- For most messages, a pass-through is sufficient. In
            -- some cases, there are side effects and UI events that
            -- need to be triggered outside the submodule.
            case subMsg of
                Form.Global.Cancel ->
                    ( nmodel
                    , Cmd.batch
                        [ sendMsg (Tab Tab.Close)
                        , Cmd.map GF subCmd
                        ]
                    )

                Form.Global.Save ->
                    let
                        jsonValue =
                            E.object
                                [ ( "email"
                                  , E.string subModel.email.value
                                  )
                                , ( "name", E.string subModel.name.value )
                                ]
                    in
                    ( nmodel
                    , Cmd.batch
                        [ sendMsg (Tab Tab.Close)
                        , updateGlobalSettings jsonValue
                        , Cmd.map GF subCmd
                        ]
                    )

                _ ->
                    ( nmodel, Cmd.map GF subCmd )

        PR id subMsg ->
            let
                vistaId =
                    "FORM::" ++ (UUID.toString id)

                oldVista =
                    case Dict.get vistaId model.vistas of
                        Nothing ->
                            { project = "global"
                            , kind = "new-project"
                            , identifier = vistaId
                            , content =
                                Content.PR (Form.Project.initData id)
                            }

                        Just vista_ ->
                            vista_
                        
                oldModel =
                    case oldVista.content of
                        Content.PR model_ ->
                            model_

                        _ ->
                            Form.Project.initData id

                ( subModel, subCmd ) =
                    Form.Project.update subMsg oldModel

                vista =
                    { oldVista | content = Content.PR subModel }

                vistas =
                    Dict.insert vistaId vista model.vistas

                nmodel =
                    { model | vistas = vistas }
            in
            -- For most messages, a pass-through is sufficient. In
            -- some cases, there are side effects and UI events that
            -- need to be triggered outside the submodule.
            case subMsg of
                Form.Project.Cancel ->
                    ( nmodel
                    , Cmd.batch
                        [ sendMsg (Tab Tab.Close)
                        , Cmd.map (PR id) subCmd
                        ]
                    )

                Form.Project.Save ->
                    let
                        jsonValue =
                            E.object
                                [ ( "title"
                                  , E.string subModel.title.value
                                  )
                                , ( "identifier", E.string
                                        (UUID.toString subModel.identifier)
                                  )
                                , ( "url", E.string subModel.url.value )
                                , ( "key", E.string subModel.key.value )
                                ]
                    in
                    ( nmodel
                    , Cmd.batch
                        [ sendMsg (Tab Tab.Close)
                        , updateProject jsonValue
                        , Cmd.map (PR id) subCmd
                        ]
                    )

                _ ->
                    ( nmodel, Cmd.map (PR id) subCmd )
                   

        Tab (Tab.New ventana) ->
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

        Tab (Tab.Goto tp) ->
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
                , sendMsg (Tab <| Tab.Focus tp)
                ]
            )

        Tab (Tab.Focus tp) ->
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

        Tab Tab.Close ->
            let
                tp =
                    Maybe.withDefault (tabpath -1 -1 -1) <| model.focused
            in
            ( closeTab True tp model, Cmd.none )

        Tab Tab.Clone ->
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
                    update (Tab <| Tab.New v) model

        Tab (Tab.Move dir) ->
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
            case D.decodeValue Config.globalConfigDecoder gc of
                Err err ->
                    ( { model | error = D.errorToString err }
                    , Cmd.none
                    )

                Ok gc_ ->
                    let
                        -- The email and name values may be blank.
                        person : Maybe M.Person
                        person =
                            let
                                h1 email name =
                                    { id = email
                                    , rev = Nothing
                                    , version = 1
                                    , names = Dict.singleton 0 name
                                    }

                                h0 email =
                                    gc_.name |> Maybe.map (h1 email)
                            in
                            gc_.email |> Maybe.andThen h0

                        newmodel =
                            { model
                                | gconfig = Just gc_
                                , me = person
                            }

                        openForm =
                            sendMsg (ShowGlobalSettings gc)

                        -- When the data is incomplere, open the form
                        -- so the user can add their name and email.
                        command =
                            case ( gc_.name, gc_.email ) of
                                ( Nothing, _ ) ->
                                    openForm

                                ( _, Nothing ) ->
                                    openForm

                                _ ->
                                    Cmd.none
                    in
                    ( newmodel, command )

        RequestInterlinearIndex id ->
            let
                id_ =
                    UUID.toString id
            in
            ( { model | loading = Set.insert id_ model.loading }
            , requestInterlinearIndex id_
            )

        -- This is not currently used
        RequestDocId project id ->
            let
                envelope =
                    envelopeEncoder
                        { command = "request-docid"
                        , project = project
                        , address = id
                        , content = E.null
                        }
            in
            ( model, send envelope )

        RequestAllDocId project id ->
            let
                envelope =
                    envelopeEncoder
                        { command = "request-all-docid"
                        , project = project
                        , address = id
                        , content = E.null
                        }
            in
            ( model, send envelope )

        ReceivedInterlinearIndex envelope ->
            case D.decodeValue envelopeDecoder envelope of
                Err e ->
                    ( { model | error = D.errorToString e }
                    , Cmd.none
                    )

                Ok env ->
                    case D.decodeValue M.listDecoder env.content of
                        Err e ->
                            ( { model | error = D.errorToString e }
                            , Cmd.none
                            )

                        Ok vals ->
                            let
                                filterInter : M.Value -> List Interlinear ->  List Interlinear
                                filterInter val ints =
                                    case val of
                                        M.MyInterlinear int ->
                                            int :: ints

                                        _ ->
                                            ints

                                content =
                                    InterlinearsContent <|
                                        List.foldl filterInter [] vals

                                vista =
                                    { project = env.project
                                    , kind = "interlinear-index"
                                    , identifier = "GLOSSES::" ++ env.project
                                    , content = content
                                    }
                            in
                            handleVista
                                vista
                                "Glosses"
                                "Glosses"
                                    model

        ReceivedAllDoc envelope ->
            case D.decodeValue envelopeDecoder envelope of
                Err e ->
                    ( { model | error = D.errorToString e }
                    , Cmd.none
                    )

                Ok env ->
                    let
                        doc =
                            reduceDoc env
                    in
                    case doc of
                        Ok doc_ ->
                            case doc_.doc of
                                Just (M.MyInterlinear i) ->
                                    let
                                        full =
                                            String.join " "
                                                [ "Gloss: ", i.text ]

                                        short =
                                            if String.length i.text > 5 then
                                                String.join ""
                                                    [ "Gloss: "
                                                    , String.left 7 i.text
                                                    , "..."
                                                    ]

                                            else
                                                full

                                        vista =
                                            { project =
                                                env.project
                                            , kind =
                                                "interlinear"
                                            , identifier =
                                                UUID.toString i.id
                                            , content =
                                                Content.ITV doc_
                                            }
                                    in
                                    handleVista vista short full model

                                _ ->
                                    ( model, Cmd.none )

                        Err e ->
                            ( { model | error = D.errorToString e }
                            , Cmd.none
                            )

        -- Open or focus the Import Options form with a filename.
        EditImporter filepath ->
            let
                projectOptions =
                    case model.gconfig of
                        Nothing ->
                            []

                        Just gconf ->
                            gconf.projects
                                |> List.map
                                    (\x ->
                                        ( x.title
                                        , UUID.toString x.identifier
                                        )
                                    )

                ( subModel, subCmd ) =
                    Form.Importer.init filepath projectOptions

                content =
                    Content.IM subModel

                vista =
                    { project = "global"
                    , kind = "import-options"
                    , identifier = "import-options"
                    , content = content
                    }

                vistas =
                    Dict.insert "import-options" vista model.vistas

                newmodel =
                    { model | vistas = vistas }
            in
            case getByVista "import-options" model.ventanas of
                Nothing ->
                    let
                        ventana =
                            { title = "Import Options"
                            , fullTitle = "Import Options"
                            , vista = "import-options"
                            , params = defVParams
                            }
                    in
                    ( newmodel
                    , Cmd.batch
                        [ sendMsg (Tab <| Tab.New ventana)
                        , Cmd.map IM subCmd
                        ]
                    )

                Just tp ->
                    ( newmodel
                    , Cmd.batch
                        [ sendMsg (Tab <| Tab.Goto tp)
                        , Cmd.map IM subCmd
                        ]
                    )

        -- Open or focus the Global Settings form with updated global
        -- configuration.
        ShowGlobalSettings value ->
            let
                ( subModel, subCmd ) =
                    Form.Global.init value

                vista =
                    { project = "global"
                    , kind = "global-settings"
                    , identifier = "global-settings"
                    , content = Content.GF subModel
                    }

                vistas =
                    Dict.insert "global-settings" vista model.vistas

                newmodel =
                    { model | vistas = vistas }
            in
            case getByVista "global-settings" model.ventanas of
                Nothing ->
                    let
                        newVentana =
                            { title = "Settings"
                            , fullTitle = "Settings"
                            , vista = "global-settings"
                            , params = defVParams
                            }
                    in
                    ( newmodel
                    , Cmd.batch
                        [ sendMsg (Tab <| Tab.New newVentana)
                        , Cmd.map GF subCmd
                        ]
                    )

                Just tp ->
                    ( newmodel
                    , Cmd.batch
                        [ sendMsg (Tab <| Tab.Goto tp)
                        , Cmd.map GF subCmd
                        ]
                    )

        NewProject ->
            let
                ( uuid, seeds ) =
                    UUID.step model.seeds

                id =
                    (UUID.toString uuid)

                vistaId =
                    "FORM::" ++ id
                        
                ( subModel, subCmd ) =
                    Form.Project.init (Form.Project.initData uuid)

                vista =
                    { project = "global"
                    , kind = "new-project"
                    , identifier = vistaId
                    , content = Content.PR subModel
                    }

                vistas =
                    Dict.insert vistaId vista model.vistas

                newmodel =
                    { model | vistas = vistas }
            in
            case getByVista vistaId model.ventanas of
                Nothing ->
                    let
                        ventana =
                            { title = "New Project"
                            , fullTitle = "New Project"
                            , vista = vistaId
                            , params = defVParams
                            }
                    in
                    ( newmodel
                    , Cmd.batch
                          [ sendMsg (Tab <| Tab.New ventana)
                          , Cmd.map (PR uuid) subCmd
                          ]
                    )

                Just tp ->
                    ( newmodel
                    , Cmd.batch
                        [ sendMsg (Tab <| Tab.Goto tp)
                        , Cmd.map (PR uuid) subCmd
                        ]
                    )

        EditProject project ->
            let
                id =
                    (UUID.toString project.identifier)
                        
                vistaId =
                    "FORM::" ++ id

                tabtitle =
                    project.title.value ++ " Settings"

                vista =
                    { project = id
                    , kind = "edit-project"
                    , identifier = vistaId
                    , content = Content.PR project
                    }

                vistas =
                    Dict.insert vistaId vista model.vistas

                newmodel =
                    { model | vistas = vistas }
            in
            case getByVista vistaId model.ventanas of
                Nothing ->
                    let
                        ventana =
                            { title = tabtitle
                            , fullTitle = tabtitle
                            , vista = vistaId
                            , params = defVParams
                            }
                    in
                    ( newmodel
                    , sendMsg (Tab <| Tab.New ventana)
                    )

                Just tp ->
                    ( newmodel
                    , sendMsg (Tab <| Tab.Goto tp)
                    )

        NewInterlinear project ->
            let
                ( uuid, seeds ) =
                    UUID.step model.seeds

                int =
                    Form.Interlinear.initData uuid

                id =
                    "FORM::" ++ UUID.toString uuid

                vista : Vista
                vista =
                    { identifier = id
                    , kind = "interlinear"
                    , project = UUID.toString project
                    , content = Content.ITE int
                    }

                ventana : Ventana
                ventana =
                    { title = "New Gloss"
                    , fullTitle = "New Gloss"
                    , vista = id
                    , params =
                        { length = 0
                        , searchString = ""
                        }
                    }
            in
            ( { model
                | seeds = seeds
                , vistas = Dict.insert id vista model.vistas
              }
            , sendMsg (Tab <| Tab.New ventana)
            )

        EditInterlinear project int ->
            let
                id =
                    "FORM::" ++ UUID.toString int.id
            in
            case getByVista id model.ventanas of
                -- The edit tab is already open.
                Just tp ->
                    ( model
                    , sendMsg (Tab <| Tab.Goto tp)
                    )

                Nothing ->
                    let
                        ( subModule, subCmd ) =
                            Form.Interlinear.init int

                        full =
                            String.join " "
                                [ "Edit: ", int.text ]

                        short =
                            if String.length int.text > 5 then
                                String.join ""
                                    [ "Edit: "
                                    , String.left 7 int.text
                                    , "..."
                                    ]

                            else
                                full

                        vista : Vista
                        vista =
                            { identifier = id
                            , kind = "interlinear"
                            , project = UUID.toString project
                            , content = Content.ITE subModule
                            }

                        ventana : Ventana
                        ventana =
                            { title = short
                            , fullTitle = full
                            , vista = id
                            , params =
                                { length = 0
                                , searchString = ""
                                }
                            }

                        vistas =
                            Dict.insert id vista model.vistas
                    in
                    ( { model | vistas = vistas }
                    , Cmd.batch
                        [ sendMsg (Tab <| Tab.New ventana)
                        , Cmd.map (ITE int.id) subCmd
                        ]
                    )

        ChangeLengthParam tp str ->
            case String.toInt str of
                Just i ->
                    case Dict.get tp model.ventanas of
                        Just ventana ->
                            let
                                params =
                                    ventana.params

                                np =
                                    { params | length = i }

                                nv =
                                    { ventana | params = np }

                                nvs =
                                    Dict.insert tp nv model.ventanas
                            in
                            ( { model | ventanas = nvs }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ChangeSearchParam tp str ->
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
                    ( { model | ventanas = nvs }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


prepInterlinearSave : Form.Interlinear.Model -> String -> Maybe M.Person -> Time.Posix -> Envelope
prepInterlinearSave int project me time =
    let
        meId =
            case me of
                Nothing ->
                    -- This should not happen, but the output may as
                    -- well supply a clue if it does.
                    "anonymous@example.com"

                Just p ->
                    p.id

        interlinear : M.Interlinear
        interlinear =
            { id = int.id
            , rev = int.rev
            , version = int.version
            , text = int.text.value
            , ann =
                { breaks = int.annotations.breaks.value
                , glosses = int.annotations.glosses.value
                , phonemic = int.annotations.phonemic.value
                , judgment = int.annotations.judgment.value
                }
            , translations =
                int.translations
                    |> List.filter (\x -> not x.deleted)
                    |> List.map
                        (\x ->
                            ( x.id
                            , { translation =
                                    x.translation.value
                              , judgment =
                                    x.judgment.value
                              }
                            )
                        )
                    |> Dict.fromList
            }

        modification : M.Modification
        modification =
            { id =
                { kind = "update"
                , docid = M.InterlinearId int.id
                , time = time
                , person = M.PersonId meId
                , fragment = []
                }
            , rev = Nothing
            , version = 1
            , comment = "No comment"
            , docversion = int.version
            , value = M.encoder (M.MyInterlinear interlinear)
            }

        envelope : Envelope
        envelope =
            { command =
                "update-doc"
            , project =
                project
            , address =
                M.identifierToString
                    (M.MyDocId <|
                        M.InterlinearId int.id
                    )
            , content =
                [ M.MyInterlinear interlinear
                , M.MyModification modification
                ]
                    |> List.map M.encoder
                    |> E.list identity
            }
    in
    envelope


handleVista : Vista -> String -> String -> Model -> ( Model, Cmd Msg )
handleVista vista short full model =
    case getProjectTitle vista.project model of
        Nothing ->
            ( { model | error = "No such project" }
            , Cmd.none
            )

        Just title ->
            let
                vistas =
                    Dict.insert vista.identifier vista model.vistas

                loading =
                    if vista.kind /= "interlinear" then
                        Set.remove vista.project model.loading

                    else
                        model.loading

                newmodel =
                    { model
                        | vistas = vistas
                        , loading = loading
                    }
            in
            case getByVista vista.identifier model.ventanas of
                Nothing ->
                    let
                        vt =
                            { title = title ++ ": " ++ short
                            , fullTitle = title ++ ": " ++ full
                            , vista = vista.identifier
                            , params = { defVParams | length = 20 }
                            }
                    in
                    ( newmodel, sendMsg (Tab <| Tab.New vt) )

                Just tp ->
                    ( newmodel, sendMsg (Tab <| Tab.Focus tp) )


{-| insertTab.Path, newTab.Path, and createNecessary are all helpers for
Move Direction. Each provides Direction specific code for some
aspect of the Move operation. This is for the case when movement
places the focused tab in a preexisting row with tabs.
-}
insertTabPath : Direction -> Tab.Path -> ( List Int, List Int ) -> List Tab.Path -> Tab.Path
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
newTabPath : Direction -> Tab.Path -> Int -> Tab.Path
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
createNecessary : Direction -> Tab.Path -> ( List Int, List Int ) -> Bool
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receivedGlobalConfig ReceivedGlobalConfig
        , receivedInterlinearIndex ReceivedInterlinearIndex
        , receivedAllDoc ReceivedAllDoc
        , newProject (\_ -> NewProject)
        , importOptions EditImporter
        , globalSettings ShowGlobalSettings
        , moveLeft_ (\_ -> Tab <| Tab.Move Left)
        , moveRight_ (\_ -> Tab <| Tab.Move Right)
        , moveUp_ (\_ -> Tab <| Tab.Move Up)
        , moveDown_ (\_ -> Tab <| Tab.Move Down)
        , closeTab_ (\_ -> Tab Tab.Close)
        , cloneTab_ (\_ -> Tab Tab.Clone)
        ]


{-| An HTML attibute that isn't included in the standard Elm library.
-}
roleAttr : String -> Html.Attribute Msg
roleAttr role =
    Attr.attribute "role" role


isInValidAttr : Bool -> Html.Attribute Msg
isInValidAttr valid =
    Attr.attribute "aria-invalid"
        (if valid then
            "false"

         else
            "true"
        )


viewUnknownProgress : List (Html.Attribute Msg) -> List (Html.Html Msg) -> Html.Html Msg
viewUnknownProgress attrs children =
    Html.node "progress" attrs children


view : Model -> Html.Html Msg
view model =
    let
        tabtree =
            treeifyTabs <| Dict.keys model.ventanas
    in
    Html.main_ []
        [ Html.aside [ Attr.class "side" ]
            [ Html.nav []
                [ Html.h4 [] [ Html.text "Projects" ]
                , viewProjects model
                , if Dict.isEmpty model.visVentanas then
                    Html.text ""

                  else
                    Html.h4 [] [ Html.text "Open Tabs" ]
                , viewTabList model.ventanas
                ]
            ]
        , if Dict.isEmpty tabtree then
            Html.div
                []
                [ if Set.isEmpty model.loading then
                    Html.h1 []
                        [ Html.text "Welcome to Your Menkayonta" ]

                  else
                    viewUnknownProgress [] []
                ]

          else
            Html.div
                [ Attr.id "content" ]
                (Dict.map (viewColumn model) tabtree
                    |> Dict.toList
                    |> List.map Tuple.second
                )
        ]


viewColumn : Model -> Int -> Dict Int (Set Int) -> Html.Html Msg
viewColumn model col rows =
    Html.div [ Attr.class "tab-column" ]
        (Dict.map (viewRow model col (Dict.size rows)) rows
            |> Dict.toList
            |> List.map Tuple.second
        )


viewRow : Model -> Int -> Int -> Int -> Set Int -> Html.Html Msg
viewRow model col _ row tabs =
    Html.div [ Attr.class "tab-row" ]
        [ Html.nav
            [ Attr.class "tab-header" ]
            (Set.toList tabs
                |> List.map (\t -> viewTabHeader model (tabpath col row t))
            )
        , Html.div [ Attr.class "tab-content" ]
            (Set.toList tabs
                |> List.map (\t -> viewTab model (tabpath col row t))
            )
        ]


viewTabHeader : Model -> Tab.Path -> Html.Html Msg
viewTabHeader model tp =
    let
        ventana =
            Dict.get tp model.ventanas
                |> Maybe.withDefault
                    { title = "Error"
                    , fullTitle = "Error"
                    , vista = "Vista not found"
                    , params = defVParams
                    }

        focused =
            Just tp == model.focused

        visible =
            visMember tp model.visVentanas
    in
    Html.button
        [ Event.onClick (Tab <| Tab.Focus tp)
        , Attr.id (tpToS tp)
        , Attr.classList
            [ ( "focused", focused )
            , ( "secondary", not focused && visible )
            , ( "secondary outline", not focused && not visible )
            , ( "tab-nav", True )
            ]
        ]
        [ Html.text ventana.title ]


viewTab : Model -> Tab.Path -> Html.Html Msg
viewTab model tp =
    Html.div
        [ Attr.classList
            [ ( "focused", Just tp == model.focused )
            , ( "hidden", not (visMember tp model.visVentanas) )
            , ( "tab-view", True )
            ]
        ]
        [ Html.div []
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
            Html.ul [] <|
                List.map (viewProject model) gconfig.projects


viewTabList : Ventanas -> Html.Html Msg
viewTabList ventanas =
    Dict.map viewTabListItem ventanas
        |> Dict.values
        |> Html.ul []


viewTabListItem : Tab.Path -> Ventana -> Html.Html Msg
viewTabListItem tp ventana =
    Html.li []
        [ Html.a
            [ Attr.href "#"
            , Event.onClick <| Tab <| Tab.Goto tp
            , Attr.class "secondary"
            ]
            [ Html.text ventana.fullTitle ]
        ]


viewLoadingProject : Model -> ProjectInfo -> Html.Html Msg
viewLoadingProject model p =
    Html.span
        [ Attr.attribute "aria-busy"
            (if Set.member (UUID.toString p.identifier) model.loading then
                "true"

             else
                "false"
            )
        ]
        [ Html.text p.title ]


viewProject : Model -> ProjectInfo -> Html.Html Msg
viewProject model p =
    let
        pmodel =
            Form.Project.fromProjectInfo p
    in
    Html.li []
        [ viewLoadingProject model p
        , Html.ul []
            [ Html.li []
                [ Html.a
                    [ Attr.href "#"
                    , Event.onClick <| EditProject pmodel
                    , Attr.class "secondary"
                    ]
                    [ Html.text "Settings" ]
                ]
            , Html.li []
                [ Html.a
                    [ Attr.href "#"
                    , Event.onClick <| RequestInterlinearIndex p.identifier
                    , Attr.class "secondary"
                    ]
                    [ Html.text "Gloss Index" ]
                ]
            , Html.li []
                [ Html.a
                    [ Attr.href "#"
                    , Event.onClick <|
                        NewInterlinear p.identifier
                    , Attr.class "secondary"
                    ]
                    [ Html.text "New Gloss" ]
                ]
            ]
        ]


viewVista : Model -> Tab.Path -> Vista -> Html Msg
viewVista model tp vista =
    case vista.content of
        Content.GF gmodel ->
            Form.Global.view gmodel |> Html.map GF

        Content.IM imodel ->
            Form.Importer.view imodel |> Html.map IM

        Content.ITE imodel ->
            Form.Interlinear.view imodel |> Html.map (ITE imodel.id)

        Content.PR pmodel ->
            Form.Project.view pmodel |> Html.map (PR pmodel.identifier)

        Content.ITV oneDoc ->
            viewOneDoc vista oneDoc

        InterlinearsContent ints ->
            let
                params =
                    Dict.get tp model.ventanas
                        |> Maybe.map .params
                        |> Maybe.withDefault { defVParams | length = 20 }

                ss =
                    params.searchString

                searched =
                    List.filter
                        (\i ->
                            String.contains ss i.text
                                || String.contains ss i.ann.glosses
                                || List.any
                                    (\t -> String.contains ss t.translation)
                                    (Dict.values i.translations)
                        )
                        ints

                total =
                    List.length searched |> String.fromInt

                is =
                    List.take params.length searched

                len =
                    String.fromInt params.length
            in
            Html.div []
                [ Html.label []
                    [ Html.text <| "Show (" ++ len ++ " of " ++ total ++ ")"
                    , Html.input
                        [ Attr.type_ "text"
                        , Attr.value len
                        , Attr.placeholder len
                        , Event.onInput (ChangeLengthParam tp)
                        ]
                        []
                    , Html.input
                        [ Attr.type_ "text"
                        , Attr.value ss
                        , Event.onInput (ChangeSearchParam tp)
                        ]
                        []
                    ]
                , Html.ol [ Attr.class "all-glosses" ]
                    (List.map (viewInterlinearIndexItem vista.project) is)
                ]


viewOneDoc : Vista -> M.OneDoc -> Html.Html Msg
viewOneDoc vista od =
    case od.doc of
        Just (M.MyInterlinear int) ->
            Html.div []
                [ Html.nav []
                    [ Html.ul []
                        [ Html.li []
                            [ Html.a
                                [ Attr.href "#"
                                , Event.onClick <|
                                    case UUID.fromString vista.project of
                                        Err _ ->
                                            None

                                        Ok uuid ->
                                            (EditInterlinear uuid int)
                                ]
                                [ Html.text "Edit" ]
                            ]
                        ]
                    ]
                , viewInterlinearOneDoc vista od int
                ]

        _ ->
            Html.div [] [ Html.text "doc not supported" ]


viewInterlinearOneDoc : Vista -> M.OneDoc -> M.Interlinear -> Html.Html Msg
viewInterlinearOneDoc vista od int =
    Html.div [ Attr.class "docview" ]
        [ Html.h2 []
            [ Html.text "Interlinear Gloss" ]
        , Html.article []
            [ viewInterlinearItem vista.project int
            , Html.footer []
                [ M.InterlinearId int.id
                    |> M.MyDocId
                    |> M.identifierToString
                    |> (++) "ID: "
                    |> Html.text
                ]
            ]
        , Html.h2 []
            [ Html.text "Metadata" ]
        , Html.div [ Attr.class "metaview" ]
            [ if List.length od.tags > 0 then
                Html.article []
                    [ Html.header []
                        [ Html.h3 []
                            [ Html.text "Tags" ]
                        ]
                    , viewTags od.tags
                    ]

              else
                Html.text ""
            , if List.length od.properties > 0 then
                Html.article []
                    [ Html.header []
                        [ Html.h3 []
                            [ Html.text "Properties" ]
                        ]
                    , viewProperties od.properties
                    ]

              else
                Html.text ""
            , if List.length od.descriptions > 0 then
                Html.article []
                    [ Html.header []
                        [ Html.h3 []
                            [ Html.text "Descriptions" ]
                        ]
                    , viewDescriptions od.descriptions
                    ]

              else
                Html.text ""
            , Html.article [ Attr.id "modification-view" ]
                [ Html.header []
                    [ Html.h3 []
                        [ Html.text "Modifications" ]
                    ]
                , viewModifications od.modifications
                ]
            ]
        ]


viewProperties : List M.Property -> Html.Html Msg
viewProperties props =
    Html.table [ Attr.class "striped" ]
        [ Html.thead []
            [ Html.tr []
                [ Html.th [ Attr.attribute "scope" "col" ]
                    [ Html.text "Attribute" ]
                , Html.th [ Attr.attribute "scope" "col" ]
                    [ Html.text "Value" ]
                ]
            ]
        , Html.tbody [] <| List.map viewProperty props
        ]


viewProperty : M.Property -> Html.Html Msg
viewProperty property =
    let
        doctype =
            case property.id.docid of
                M.InterlinearId _ ->
                    "interlinear"

                M.PersonId _ ->
                    "person"
    in
    Html.tr []
        [ Html.td []
            [ Html.text property.id.kind ]
        , Html.td []
            [ Html.text property.id.value ]
        ]


viewModifications : List M.Modification -> Html.Html Msg
viewModifications mods =
    Html.table [ Attr.class "striped" ]
        [ Html.thead []
            [ Html.tr []
                [ Html.th [ Attr.attribute "scope" "col" ]
                    [ Html.text "Event" ]
                , Html.th [ Attr.attribute "scope" "col" ]
                    [ Html.text "Time" ]
                ]
            ]
        , Html.tbody [] <|
            List.map viewModification <|
                List.reverse <|
                    List.sortBy (\m -> Time.posixToMillis m.id.time) mods
        ]


viewModification : M.Modification -> Html.Html Msg
viewModification modification =
    let
        doctype =
            case modification.id.docid of
                M.InterlinearId _ ->
                    "interlinear"

                M.PersonId _ ->
                    "person"
    in
    Html.tr []
        [ Html.td []
            [ Html.text modification.id.kind ]
        , Html.td []
            [ Html.text <| Iso8601.fromTime modification.id.time ]
        ]


viewTags : List M.Tag -> Html.Html Msg
viewTags tags =
    Html.div [] <|
        List.map viewTag tags


viewTag : M.Tag -> Html.Html Msg
viewTag tag =
    let
        doctype =
            case tag.id.docid of
                M.InterlinearId _ ->
                    "interlinear"

                M.PersonId _ ->
                    "person"
    in
    Html.span [ Attr.class "tag" ] [ Html.text tag.id.kind ]


viewDescriptions : List M.Description -> Html.Html Msg
viewDescriptions descriptions =
    Html.div [] <|
        List.map viewDescription descriptions


viewDescription : M.Description -> Html.Html Msg
viewDescription description =
    let
        doctype =
            case description.id.docid of
                M.InterlinearId _ ->
                    "interlinear"

                M.PersonId _ ->
                    "person"
    in
    Html.details []
        [ Html.summary []
            [ Html.text description.id.kind ]
        , Html.p []
            [ Html.text description.value ]
        ]


viewInterlinearIndexItem : String -> M.Interlinear -> Html.Html Msg
viewInterlinearIndexItem proj int =
    Html.li []
        [ viewInterlinearItem proj int
        , Html.a
            [ Attr.href "#"
            , Event.onClick <|
                RequestAllDocId proj <|
                    String.join ""
                        [ "interlinear/"
                        , UUID.toString int.id
                        ]
            ]
            [ Html.text "Open" ]
        ]


viewInterlinearItem : String -> M.Interlinear -> Html.Html Msg
viewInterlinearItem proj int =
    let
        srcLine =
            if int.ann.breaks /= "" then
                viewAnn int.ann.judgment int.text int.ann.breaks int.ann.glosses

            else
                Html.p [] [ Html.text (int.ann.judgment ++ " " ++ int.text) ]

        transLines =
            List.map (\t -> Html.p [] [ Html.text t.translation ]) (Dict.values int.translations)
    in
    Html.div [] (srcLine :: transLines)


viewAnn : String -> String -> String -> String -> Html.Html Msg
viewAnn jdg src brk gls =
    let
        src_ =
            jdg :: String.split " " src

        brk_ =
            "" :: String.split " " brk

        gls_ =
            "" :: String.split " " gls

        aligned =
            LE.zip3 src_ brk_ gls_
    in
    List.map viewGlossTriple aligned
        |> Html.div [ Attr.class "aligned-glosses" ]


viewGlossTriple : ( String, String, String ) -> Html.Html Msg
viewGlossTriple ( a, b, c ) =
    Html.div [ Attr.class "gloss-column" ]
        [ Html.div [] [ Html.text a ]
        , Html.div [] [ Html.text b ]
        , Html.div [] [ Html.text c ]
        ]


{-| Return the Tab.Paths for the tabs in the same row.
-}
sharesRow : Tab.Path -> Model -> List Tab.Path
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
nearest : Tab.Path -> Model -> Maybe Tab.Path
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
closeTab : Bool -> Tab.Path -> Model -> Model
closeTab closevista tp model =
    let
        gvistas =
            Dict.keys globalVistas

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
reassign : Tab.Path -> Tab.Path -> Model -> Model
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
treeifyTabs : List Tab.Path -> Dict Int (Dict Int (Set Int))
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


tabpath : Int -> Int -> Int -> Tab.Path
tabpath c r t =
    ( c, ( r, t ) )


tpToS : Tab.Path -> String
tpToS ( c, ( r, t ) ) =
    [ c, r, t ]
        |> List.map String.fromInt
        |> String.join ","


tcolumn : Tab.Path -> Int
tcolumn tp =
    Tuple.first tp


trow : Tab.Path -> Int
trow tp =
    tp |> Tuple.second |> Tuple.first


ttab : Tab.Path -> Int
ttab tp =
    tp |> Tuple.second |> Tuple.second


{-| All columns in order
-}
tcolumns : List Tab.Path -> List Int
tcolumns tps =
    List.map tcolumn tps
        |> LE.unique
        |> List.sort


{-| Rows for a column in order
-}
trows : Int -> List Tab.Path -> List Int
trows column tps =
    List.filter (\tp -> tcolumn tp == column) tps
        |> List.map trow
        |> LE.unique
        |> List.sort


{-| Insert a tab into VisVentanas
-}
visInsert : Tab.Path -> VisVentanas -> VisVentanas
visInsert tp vv =
    Dict.insert ( tcolumn tp, trow tp ) (ttab tp) vv


{-| Insert a tab into VisVentanas
-}
visRemove : Tab.Path -> VisVentanas -> VisVentanas
visRemove tp vv =
    Dict.remove ( tcolumn tp, trow tp ) vv


visToList : VisVentanas -> List Tab.Path
visToList vv =
    Dict.toList vv
        |> List.map (\( ( c, r ), t ) -> ( c, ( r, t ) ))


visMember : Tab.Path -> VisVentanas -> Bool
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



-- I'm doing a one off here, instead of adding it to the Menkayonta
-- module because I want to eventually be able to handle all
-- Menkayonta Values in the UI.


interlinearDecoder : D.Decoder Interlinear
interlinearDecoder =
    let
        checkval value_ =
            case value_ of
                M.MyInterlinear inter ->
                    D.succeed inter

                _ ->
                    D.fail "Non-Interlinear Value"
    in
    M.decoder |> D.andThen checkval


contentDecoder : String -> D.Decoder Content
contentDecoder kind =
    case kind of
        "all-interlinears" ->
            D.map InterlinearsContent
                (D.field "content" <| D.list interlinearDecoder)

        _ ->
            D.fail ("Unsupported content kind " ++ kind)


getVistaVentana : Tab.Path -> Model -> Maybe ( Vista, Ventana )
getVistaVentana tp model =
    Dict.get tp model.ventanas
        |> Maybe.andThen
            (\ventana ->
                Dict.get ventana.vista model.vistas
                    |> Maybe.map (\vista -> ( vista, ventana ))
            )


getContentVistaVentana : Tab.Path -> Model -> Maybe ( Content, ( Vista, Ventana ) )
getContentVistaVentana tp model =
    getVistaVentana tp model
        |> Maybe.map (\( vis, ven ) -> ( vis.content, ( vis, ven ) ))


getContentVistaFromVistas : String -> Vistas -> Maybe ( Content, Vista )
getContentVistaFromVistas vid vistas =
    Dict.get vid vistas
        |> Maybe.map (\vista -> ( vista.content, vista ))


getByVista : String -> Dict Tab.Path Ventana -> Maybe Tab.Path
getByVista vista ventanas =
    List.head <| getAllByVista vista ventanas


getAllByVista : String -> Dict Tab.Path Ventana -> List Tab.Path
getAllByVista vista ventanas =
    -- TODO use Dict.filter (this may not warrant being a helper
    -- function.)
    List.filter (\( _, v ) -> v.vista == vista) (Dict.toList ventanas)
        |> List.map Tuple.first


getProjectTitle : String -> Model -> Maybe String
getProjectTitle projid model =
    let
        projuuid =
            UUID.fromString projid |> Result.toMaybe

        projects =
            Maybe.withDefault
                {email = Nothing, name = Nothing, projects = []} model.gconfig
                    |> .projects
    in
    LE.find (\x -> Just x.identifier == projuuid) projects
        |> Maybe.map .title


reduceDoc : Envelope -> Result D.Error M.OneDoc
reduceDoc env =
    let
        content =
            env.content |> D.decodeValue M.listDecoder

        initial =
            M.OneDoc Nothing [] [] [] []
    in
    case content of
        Ok content_ ->
            Ok <| List.foldl M.oneBuilder initial content_

        Err e ->
            Err e



{- PORTS -}


port send : E.Value -> Cmd msg


{-| The window title changes depending on the focused tab. This sends
the signal to the backend to do so.
-}
port setWindowTitle : String -> Cmd msg


{-| The project index is a listing of all translated items with their
translations, which serves as an entry point to a project. This port
requests the index.
-}
port requestProjectIndex : String -> Cmd msg


port requestInterlinearIndex : String -> Cmd msg


port requestPersonIndex : String -> Cmd msg


{-| The global configuration lists projects and whether they are
enabled. It may also include other configuration information. This
port is used to request the global configuration.
-}
port requestGlobalConfig : () -> Cmd msg


{-| The global configuration lists projects and whether they are
enabled. It may also include other configuration information. This
port indicates that the global configuration was received.
-}
port receivedGlobalConfig : (E.Value -> msg) -> Sub msg


port receivedInterlinearIndex : (E.Value -> msg) -> Sub msg


port receivedAllDoc : (E.Value -> msg) -> Sub msg


{-| The "New Project" menu item was clicked.
-}
port newProject : (() -> msg) -> Sub msg


{-| The "Settings" menu item was clicked.
-}
port globalSettings : (E.Value -> msg) -> Sub msg


{-| The "Import File" menu item was clicked.
-}
port importOptions : (String -> msg) -> Sub msg


{-| Send ProjectInfo to the backend for update.
-}
port updateProject : E.Value -> Cmd msg


{-| Send ImportOptions to the backend for execution.
-}
port importFile : E.Value -> Cmd msg


{-| Send the portion of the global configuration that does not include
project configuration to the backend.
-}
port updateGlobalSettings : E.Value -> Cmd msg


port moveLeft_ : (() -> msg) -> Sub msg


port moveRight_ : (() -> msg) -> Sub msg


port moveUp_ : (() -> msg) -> Sub msg


port moveDown_ : (() -> msg) -> Sub msg


port closeTab_ : (() -> msg) -> Sub msg


port cloneTab_ : (() -> msg) -> Sub msg
