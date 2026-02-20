port module Main exposing (Dimensions, Flags, Model, Msg, main)

import Browser
import Config exposing (GlobalConfig, ProjectInfo)
import Content exposing (Content(..))
import Dict exposing (Dict)
import Display.InterlinearListing
import Display.Composite
import Form.Global
import Form.Importer
import Form.Interlinear
import Form.Project
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import Iso8601
import Json.Decode as D
import Json.Encode as E
import List.Extra as LE
import Menkayonta as M
import Random
import Set exposing (Set)
import Tab
    exposing
        ( Direction(..)
        , Ventana
        , VentanaParams
        , Ventanas
        , Vista
        , columnCount
        , defVParams
        , getByVista
        , multipleRows
        , pathToString
        , tabpath
        , treeifyTabs
        , visMember
        )
import Task
import Time
import UUID


type alias Model =
    { gconfig : GlobalConfig
    , me : Maybe M.Person
    , tabs : Tab.Model
    , error : String
    , loading : Set String
    , seeds : UUID.Seeds
    , sideBar : Bool
    , viewArea : Dimensions
    }


type Msg
    = GF Form.Global.Msg
    | IM Form.Importer.Msg
    | ITE UUID.UUID Form.Interlinear.Msg
    | EditImporter String
    | MultiMsg (List Msg)
    | None
    | PR UUID.UUID Form.Project.Msg
    | ReceivedAllDoc E.Value
    | ReceivedGlobalConfig E.Value
    | ReceivedInterlinearIndex E.Value
    | Received ReceiveType
    | RequestAllDocId String String
      -- | RequestDocId String String
    | RequestInterlinearIndex UUID.UUID
      -- | SetWindowTitle String
    | ShowGlobalSettings E.Value
    | NewInterlinear UUID.UUID
    | EditInterlinear UUID.UUID M.Interlinear
    | NewProject
    | EditProject Form.Project.Model
    | Stamp (Time.Posix -> Envelope) (E.Value -> Cmd Msg) Time.Posix
    | Tab Tab.Msg
    | ToggleSidebar
    | UserClick Msg


{-| Inject a message into `Cmd`
-}
sendMsg : Msg -> Cmd Msg
sendMsg msg =
    Task.succeed msg
        |> Task.perform identity


type ReceiveType
    = ViewArea E.Value


type alias Dimensions =
    { length : Int
    , width : Int
    }


dimensionsDecoder : D.Decoder Dimensions
dimensionsDecoder =
    D.map2 Dimensions
        (D.field "length" D.int)
        (D.field "width" D.int)


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
    { seeds :
        { seed1 : Int
        , seed2 : Int
        , seed3 : Int
        , seed4 : Int
        }
    , dimensions :
        { length : Int
        , width : Int
        }
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        seeds : UUID.Seeds
        seeds =
            { seed1 = Random.initialSeed flags.seeds.seed1
            , seed2 = Random.initialSeed flags.seeds.seed2
            , seed3 = Random.initialSeed flags.seeds.seed3
            , seed4 = Random.initialSeed flags.seeds.seed4
            }

        g :
            { model : Form.Global.Model
            , cmd : Cmd Form.Global.Msg
            }
        g =
            let
                ( gmodel, gcmd ) =
                    Form.Global.init E.null
            in
            { model = gmodel, cmd = gcmd }

        gv : Dict String Vista
        gv =
            Dict.get "global-settings" globalVistas
                |> Maybe.map
                    (\gs ->
                        { gs
                            | content = Content.GF g.model
                        }
                    )
                |> Maybe.map
                    (\gs -> Dict.insert "global-settings" gs globalVistas)
                |> Maybe.withDefault globalVistas
    in
    ( { gconfig = { email = "", name = "", projects = [] }
      , me = Nothing
      , tabs = Tab.initData gv
      , error = ""
      , loading = Set.empty
      , seeds = seeds
      , sideBar = True
      , viewArea = flags.dimensions
      }
    , Cmd.batch
        [ requestGlobalConfig ()
        , Cmd.map GF g.cmd
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
                envelope : E.Value
                envelope =
                    envelopePart time |> envelopeEncoder
            in
            ( model, cmd envelope )

        UserClick clickMsg ->
            let
                subModel : Tab.Model
                subModel =
                    Tab.update Tab.Unlock model.tabs
                        |> Tuple.first
            in
            ( { model | tabs = subModel }, sendMsg clickMsg )

        ToggleSidebar ->
            ( { model | sideBar = not model.sideBar }
            , Cmd.none
            )

        Tab subMsg ->
            let
                t :
                    { model : Tab.Model
                    , cmd : Cmd Tab.Msg
                    }
                t =
                    subUpdate Tab.update subMsg model.tabs
            in
            ( { model | tabs = t.model }
            , Cmd.map Tab t.cmd
            )

        Received (ViewArea jsonValue) ->
            case D.decodeValue dimensionsDecoder jsonValue of
                Err e ->
                    ( { model | error = D.errorToString e }
                    , Cmd.none
                    )

                Ok dimensions ->
                    ( { model | viewArea = dimensions }
                    , Cmd.none
                    )

        ITE id subMsg ->
            let
                id_ : String
                id_ =
                    "FORM::" ++ UUID.toString id

                maybeVista : Maybe Vista
                maybeVista =
                    Dict.get id_ model.tabs.vistas

                maybeTab : Maybe Tab.Path
                maybeTab =
                    getByVista id_ model.tabs.ventanas
            in
            case ( maybeVista, maybeTab ) of
                ( Nothing, _ ) ->
                    -- Something is wrong. Ignore the message.
                    ( model, Cmd.none )

                ( _, Nothing ) ->
                    -- Something is wrong. Ignore the message.
                    ( model, Cmd.none )

                ( Just vista_, Just tp ) ->
                    let
                        oldModel : Form.Interlinear.Model
                        oldModel =
                            case vista_.content of
                                Content.ITE model_ ->
                                    model_

                                _ ->
                                    Form.Interlinear.initData id

                        i :
                            { model : Form.Interlinear.Model
                            , cmd : Cmd Form.Interlinear.Msg
                            }
                        i =
                            subUpdate
                                Form.Interlinear.update
                                subMsg
                                oldModel

                        vista : Vista
                        vista =
                            { vista_ | content = Content.ITE i.model }

                        vistas : Dict String Vista
                        vistas =
                            Dict.insert id_ vista model.tabs.vistas

                        tabs : Tab.Model
                        tabs =
                            model.tabs

                        nmodel : Model
                        nmodel =
                            { model | tabs = { tabs | vistas = vistas } }
                    in
                    -- For most messages, a pass-through is
                    -- sufficient. In some cases, there are side
                    -- effects and UI events that need to be triggered
                    -- outside the submodule.
                    case subMsg of
                        Form.Interlinear.Cancel ->
                            ( nmodel
                            , Cmd.batch
                                [ sendMsg (Tab (Tab.Close tp))
                                , Cmd.map (ITE id) i.cmd
                                ]
                            )

                        Form.Interlinear.Save ->
                            let
                                envelopePart : Time.Posix -> Envelope
                                envelopePart =
                                    prepInterlinearSave
                                        i.model
                                        vista.project
                                        model.me
                            in
                            ( nmodel
                            , Cmd.batch
                                [ sendMsg (Tab (Tab.Close tp))
                                , Task.perform
                                    (Stamp envelopePart send)
                                    Time.now
                                , Cmd.map (ITE id) i.cmd
                                ]
                            )

                        _ ->
                            ( nmodel, Cmd.map (ITE id) i.cmd )

        IM subMsg ->
            let
                maybeTab : Maybe Tab.Path
                maybeTab =
                    getByVista "import-options" model.tabs.ventanas
            in
            case maybeTab of
                Nothing ->
                    ( model, Cmd.none )

                Just tp ->
                    let
                        oldModel : Form.Importer.Model
                        oldModel =
                            Dict.get "import-options" model.tabs.vistas
                                |> Maybe.map .content
                                |> (\c ->
                                        case c of
                                            Just (Content.IM model_) ->
                                                model_

                                            _ ->
                                                -- this has no
                                                -- filepath, so it
                                                -- will do nothing.
                                                Form.Importer.initData
                                   )

                        i :
                            { model : Form.Importer.Model
                            , cmd : Cmd Form.Importer.Msg
                            }
                        i =
                            subUpdate Form.Importer.update subMsg oldModel

                        vista : Vista
                        vista =
                            { importOptionsVista
                                | content = Content.IM i.model
                            }

                        vistas : Dict String Vista
                        vistas =
                            Dict.insert
                                "import-options"
                                vista
                                model.tabs.vistas

                        tabs : Tab.Model
                        tabs =
                            model.tabs

                        nmodel : Model
                        nmodel =
                            { model | tabs = { tabs | vistas = vistas } }
                    in
                    -- For most messages, a pass-through is
                    -- sufficient. In some cases, there are side
                    -- effects and UI events that need to be triggered
                    -- outside the submodule.
                    case subMsg of
                        Form.Importer.Cancel ->
                            ( nmodel
                            , Cmd.batch
                                [ sendMsg (Tab (Tab.Close tp))
                                , Cmd.map IM i.cmd
                                ]
                            )

                        Form.Importer.Import ->
                            let
                                jsonValue : E.Value
                                jsonValue =
                                    E.object
                                        [ ( "filepath"
                                          , E.string i.model.filepath
                                          )
                                        , ( "kind"
                                          , E.string i.model.kind.value
                                          )
                                        , ( "project"
                                          , E.string i.model.project.value
                                          )
                                        ]
                            in
                            ( nmodel
                            , Cmd.batch
                                [ sendMsg (Tab (Tab.Close tp))
                                , importFile jsonValue
                                , Cmd.map IM i.cmd
                                ]
                            )

                        _ ->
                            ( nmodel, Cmd.map IM i.cmd )

        GF subMsg ->
            let
                maybeTab : Maybe Tab.Path
                maybeTab =
                    getByVista "global-settings" model.tabs.ventanas
            in
            case maybeTab of
                Nothing ->
                    ( model, Cmd.none )

                Just tp ->
                    let
                        oldModel : Form.Global.Model
                        oldModel =
                            Dict.get "global-settings" model.tabs.vistas
                                |> Maybe.map .content
                                |> (\c ->
                                        case c of
                                            Just (Content.GF model_) ->
                                                model_

                                            _ ->
                                                Form.Global.initData
                                   )

                        g :
                            { model : Form.Global.Model
                            , cmd : Cmd Form.Global.Msg
                            }
                        g =
                            subUpdate Form.Global.update subMsg oldModel

                        vista : Vista
                        vista =
                            { globalSettingsVista
                                | content = Content.GF g.model
                            }

                        vistas : Dict String Vista
                        vistas =
                            Dict.insert
                                "global-settings"
                                vista
                                model.tabs.vistas

                        tabs : Tab.Model
                        tabs =
                            model.tabs

                        nmodel : Model
                        nmodel =
                            { model | tabs = { tabs | vistas = vistas } }
                    in
                    -- For most messages, a pass-through is
                    -- sufficient. In some cases, there are side
                    -- effects and UI events that need to be triggered
                    -- outside the submodule.
                    case subMsg of
                        Form.Global.Cancel ->
                            ( nmodel
                            , Cmd.batch
                                [ sendMsg (Tab (Tab.Close tp))
                                , Cmd.map GF g.cmd
                                ]
                            )

                        Form.Global.Save ->
                            let
                                jsonValue : E.Value
                                jsonValue =
                                    E.object
                                        [ ( "email"
                                          , E.string g.model.email.value
                                          )
                                        , ( "name"
                                          , E.string g.model.name.value
                                          )
                                        ]
                            in
                            ( nmodel
                            , Cmd.batch
                                [ sendMsg (Tab (Tab.Close tp))
                                , updateGlobalSettings jsonValue
                                , Cmd.map GF g.cmd
                                ]
                            )

                        _ ->
                            ( nmodel, Cmd.map GF g.cmd )

        PR id subMsg ->
            let
                vistaId : String
                vistaId =
                    "FORM::" ++ UUID.toString id

                maybeTab : Maybe Tab.Path
                maybeTab =
                    getByVista vistaId model.tabs.ventanas
            in
            case maybeTab of
                Nothing ->
                    ( model, Cmd.none )

                Just tp ->
                    let
                        oldVista : Vista
                        oldVista =
                            case Dict.get vistaId model.tabs.vistas of
                                Nothing ->
                                    { project = "global"
                                    , kind = "new-project"
                                    , identifier = vistaId
                                    , content =
                                        Content.PR
                                            (Form.Project.initData id)
                                    }

                                Just vista_ ->
                                    vista_

                        oldModel : Form.Project.Model
                        oldModel =
                            case oldVista.content of
                                Content.PR model_ ->
                                    model_

                                _ ->
                                    Form.Project.initData id

                        p :
                            { model : Form.Project.Model
                            , cmd : Cmd Form.Project.Msg
                            }
                        p =
                            subUpdate Form.Project.update subMsg oldModel

                        vista : Vista
                        vista =
                            { oldVista | content = Content.PR p.model }

                        vistas : Dict String Vista
                        vistas =
                            Dict.insert vistaId vista model.tabs.vistas

                        tabs : Tab.Model
                        tabs =
                            model.tabs

                        nmodel : Model
                        nmodel =
                            { model | tabs = { tabs | vistas = vistas } }
                    in
                    -- For most messages, a pass-through is
                    -- sufficient. In some cases, there are side
                    -- effects and UI events that need to be triggered
                    -- outside the submodule.
                    case subMsg of
                        Form.Project.Cancel ->
                            ( nmodel
                            , Cmd.batch
                                [ sendMsg (Tab (Tab.Close tp))
                                , Cmd.map (PR id) p.cmd
                                ]
                            )

                        Form.Project.Save ->
                            let
                                jsonValue : E.Value
                                jsonValue =
                                    E.object
                                        [ ( "title"
                                          , E.string p.model.title.value
                                          )
                                        , ( "identifier"
                                          , E.string
                                                (UUID.toString
                                                    p.model.identifier
                                                )
                                          )
                                        , ( "url"
                                          , E.string p.model.url.value
                                          )
                                        , ( "key"
                                          , E.string p.model.key.value
                                          )
                                        ]
                            in
                            ( nmodel
                            , Cmd.batch
                                [ sendMsg (Tab (Tab.Close tp))
                                , updateProject jsonValue
                                , Cmd.map (PR id) p.cmd
                                ]
                            )

                        _ ->
                            ( nmodel, Cmd.map (PR id) p.cmd )

        -- SetWindowTitle title ->
        --     ( model, setWindowTitle title )
        ReceivedGlobalConfig gc ->
            case D.decodeValue Config.globalConfigDecoder gc of
                Err err ->
                    ( { model | error = D.errorToString err }
                    , Cmd.none
                    )

                Ok gc_ ->
                    let
                        invalidPerson : Bool
                        invalidPerson =
                            String.isEmpty gc_.name
                                || String.isEmpty gc_.email

                        newmodel : Model
                        newmodel =
                            { model
                                | gconfig = gc_
                                , me =
                                    if invalidPerson then
                                        Nothing

                                    else
                                        Just
                                            { id = gc_.email
                                            , rev = Nothing
                                            , version = 1
                                            , names =
                                                Dict.singleton 0 gc_.name
                                            }
                            }

                        -- When the data is incomplete, open the form
                        -- so the user can add their name and email.
                        command : Cmd Msg
                        command =
                            if invalidPerson then
                                sendMsg (ShowGlobalSettings gc)

                            else
                                Cmd.none
                    in
                    ( newmodel, command )

        RequestInterlinearIndex id ->
            let
                id_ : String
                id_ =
                    UUID.toString id
            in
            ( { model | loading = Set.insert id_ model.loading }
            , requestInterlinearIndex id_
            )

        -- This is not currently used
        -- RequestDocId project id ->
        --     let
        --         envelope : E.Value
        --         envelope =
        --             envelopeEncoder
        --                 { command = "request-docid"
        --                 , project = project
        --                 , address = id
        --                 , content = E.null
        --                 }
        --     in
        --     ( model, send envelope )
        RequestAllDocId project id ->
            let
                envelope : E.Value
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
                                filterInter :
                                    M.Value
                                    -> List M.Interlinear
                                    -> List M.Interlinear
                                filterInter val ints =
                                    case val of
                                        M.MyInterlinear int ->
                                            int :: ints

                                        _ ->
                                            ints

                                content : Content
                                content =
                                    ITS <|
                                        List.foldl filterInter [] vals

                                vista : Vista
                                vista =
                                    { project =
                                        env.project
                                    , kind =
                                        "interlinear-index"
                                    , identifier =
                                        "GLOSSES::" ++ env.project
                                    , content =
                                        content
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
                        doc : Result.Result D.Error M.Composite
                        doc =
                            reduceDoc env
                    in
                    case doc of
                        Ok doc_ ->
                            case doc_.doc of
                                Just (M.MyInterlinear i) ->
                                    let
                                        full : String
                                        full =
                                            String.join " "
                                                [ "Gloss:", i.text ]

                                        short : String
                                        short =
                                            if String.length i.text > 5 then
                                                String.concat
                                                    [ "Gloss: "
                                                    , String.left 7 i.text
                                                    , "..."
                                                    ]

                                            else
                                                full

                                        vista : Vista
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
                i :
                    { model : Form.Importer.Model
                    , cmd : Cmd Form.Importer.Msg
                    }
                i =
                    let
                        projectOptions : List ( String, String )
                        projectOptions =
                            model.gconfig.projects
                                |> List.map
                                    (\x ->
                                        ( x.title
                                        , UUID.toString x.identifier
                                        )
                                    )

                        ( imodel, icmd ) =
                            Form.Importer.init filepath projectOptions
                    in
                    { model = imodel, cmd = icmd }

                content : Content
                content =
                    Content.IM i.model

                vista : Vista
                vista =
                    { project = "global"
                    , kind = "import-options"
                    , identifier = "import-options"
                    , content = content
                    }

                vistas : Dict String Vista
                vistas =
                    Dict.insert "import-options" vista model.tabs.vistas

                tabs : Tab.Model
                tabs =
                    model.tabs

                newmodel : Model
                newmodel =
                    { model
                        | tabs =
                            { tabs
                                | vistas = vistas
                                , focusLock = Nothing
                            }
                    }
            in
            case getByVista "import-options" model.tabs.ventanas of
                Nothing ->
                    let
                        ventana : Ventana
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
                        , Cmd.map IM i.cmd
                        ]
                    )

                Just tp ->
                    ( newmodel
                    , Cmd.batch
                        [ sendMsg (Tab <| Tab.Goto tp)
                        , Cmd.map IM i.cmd
                        ]
                    )

        -- Open or focus the Global Settings form with updated global
        -- configuration.
        ShowGlobalSettings value ->
            let
                g :
                    { model : Form.Global.Model
                    , cmd : Cmd Form.Global.Msg
                    }
                g =
                    let
                        ( gmodel, gcmd ) =
                            Form.Global.init value
                    in
                    { model = gmodel, cmd = gcmd }

                vista : Vista
                vista =
                    { project = "global"
                    , kind = "global-settings"
                    , identifier = "global-settings"
                    , content = Content.GF g.model
                    }

                vistas : Dict String Vista
                vistas =
                    Dict.insert "global-settings" vista model.tabs.vistas

                tabs : Tab.Model
                tabs =
                    model.tabs

                newmodel : Model
                newmodel =
                    { model
                        | tabs =
                            { tabs
                                | vistas = vistas
                                , focusLock = Nothing
                            }
                    }
            in
            case getByVista "global-settings" model.tabs.ventanas of
                Nothing ->
                    let
                        newVentana : Ventana
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
                        , Cmd.map GF g.cmd
                        ]
                    )

                Just tp ->
                    ( newmodel
                    , Cmd.batch
                        [ sendMsg (Tab <| Tab.Goto tp)
                        , Cmd.map GF g.cmd
                        ]
                    )

        NewProject ->
            let
                step :
                    { uuid : UUID.UUID
                    , seeds : UUID.Seeds
                    }
                step =
                    let
                        ( uuid, seeds ) =
                            UUID.step model.seeds
                    in
                    { uuid = uuid, seeds = seeds }

                id : String
                id =
                    UUID.toString step.uuid

                vistaId : String
                vistaId =
                    "FORM::" ++ id

                p :
                    { model : Form.Project.Model
                    , cmd : Cmd Form.Project.Msg
                    }
                p =
                    let
                        ( pmodel, pcmd ) =
                            Form.Project.init
                                (Form.Project.initData step.uuid)
                    in
                    { model = pmodel, cmd = pcmd }

                vista : Vista
                vista =
                    { project = "global"
                    , kind = "new-project"
                    , identifier = vistaId
                    , content = Content.PR p.model
                    }

                vistas : Dict String Vista
                vistas =
                    Dict.insert vistaId vista model.tabs.vistas

                tabs : Tab.Model
                tabs =
                    model.tabs

                newmodel : Model
                newmodel =
                    { model
                        | tabs = { tabs | vistas = vistas }
                        , seeds = step.seeds
                    }
            in
            case getByVista vistaId model.tabs.ventanas of
                Nothing ->
                    let
                        ventana : Ventana
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
                        , Cmd.map (PR step.uuid) p.cmd
                        ]
                    )

                Just tp ->
                    ( newmodel
                    , Cmd.batch
                        [ sendMsg (Tab <| Tab.Goto tp)
                        , Cmd.map (PR step.uuid) p.cmd
                        ]
                    )

        EditProject project ->
            let
                id : String
                id =
                    UUID.toString project.identifier

                vistaId : String
                vistaId =
                    "FORM::" ++ id

                vista : Vista
                vista =
                    { project = id
                    , kind = "edit-project"
                    , identifier = vistaId
                    , content = Content.PR project
                    }

                vistas : Dict String Vista
                vistas =
                    Dict.insert vistaId vista model.tabs.vistas

                tabs : Tab.Model
                tabs =
                    model.tabs

                newmodel : Model
                newmodel =
                    { model | tabs = { tabs | vistas = vistas } }
            in
            case getByVista vistaId model.tabs.ventanas of
                Nothing ->
                    let
                        tabtitle : String
                        tabtitle =
                            project.title.value ++ " Settings"

                        ventana : Ventana
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
                    , Cmd.batch
                        [ sendMsg (Tab <| Tab.Goto tp)
                        , sendMsg (Tab <| Tab.Select tp)
                        ]
                    )

        NewInterlinear project ->
            let
                step :
                    { uuid : UUID.UUID
                    , seeds : UUID.Seeds
                    }
                step =
                    let
                        ( uuid, seeds ) =
                            UUID.step model.seeds
                    in
                    { uuid = uuid, seeds = seeds }

                int : Form.Interlinear.Model
                int =
                    Form.Interlinear.initData step.uuid

                id : String
                id =
                    "FORM::" ++ UUID.toString step.uuid

                vista : Vista
                vista =
                    { identifier = id
                    , kind = "interlinear"
                    , project = UUID.toString project
                    , content = Content.ITE int
                    }

                key : String
                key =
                    getProjectKey (UUID.toString project) model
                        |> Maybe.withDefault ""

                title : String
                title =
                    getProjectTitle (UUID.toString project) model
                        |> Maybe.withDefault ""

                ventana : Ventana
                ventana =
                    { title = key ++ ": New Gloss"
                    , fullTitle = title ++ ": New Gloss"
                    , vista = id
                    , params =
                        { length = 0
                        , searchString = ""
                        }
                    }

                tabs : Tab.Model
                tabs =
                    model.tabs

                vistas : Dict String Vista
                vistas =
                    Dict.insert id vista model.tabs.vistas

                newmodel : Model
                newmodel =
                    { model
                        | seeds = step.seeds
                        , tabs = { tabs | vistas = vistas }
                    }
            in
            ( newmodel
            , sendMsg (Tab <| Tab.New ventana)
            )

        EditInterlinear project int ->
            let
                id : String
                id =
                    "FORM::" ++ UUID.toString int.id
            in
            case getByVista id model.tabs.ventanas of
                -- The edit tab is already open.
                Just tp ->
                    ( model
                    , sendMsg (Tab <| Tab.Goto tp)
                    )

                Nothing ->
                    let
                        i :
                            { model : Form.Interlinear.Model
                            , cmd : Cmd Form.Interlinear.Msg
                            }
                        i =
                            let
                                ( imodel, icmd ) =
                                    Form.Interlinear.init int
                            in
                            { model = imodel, cmd = icmd }

                        key : String
                        key =
                            getProjectKey (UUID.toString project) model
                                |> Maybe.withDefault ""

                        title : String
                        title =
                            getProjectTitle (UUID.toString project) model
                                |> Maybe.withDefault ""

                        full : String
                        full =
                            String.concat
                                [ title
                                , ": "
                                , "Edit: "
                                , int.text
                                ]

                        short : String
                        short =
                            if String.length int.text > 5 then
                                String.concat
                                    [ key
                                    , ": "
                                    , "Edit: "
                                    , String.left 7 int.text
                                    , "..."
                                    ]

                            else
                                String.concat
                                    [ key
                                    , ": "
                                    , "Edit: "
                                    , int.text
                                    ]

                        vista : Vista
                        vista =
                            { identifier = id
                            , kind = "interlinear"
                            , project = UUID.toString project
                            , content = Content.ITE i.model
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

                        vistas : Dict String Vista
                        vistas =
                            Dict.insert id vista model.tabs.vistas

                        tabs : Tab.Model
                        tabs =
                            model.tabs

                        newmodel : Model
                        newmodel =
                            { model
                                | tabs =
                                    { tabs
                                        | vistas = vistas
                                        , focusLock = Nothing
                                    }
                            }
                    in
                    ( newmodel
                    , Cmd.batch
                        [ sendMsg (Tab <| Tab.New ventana)
                        , Cmd.map (ITE int.id) i.cmd
                        ]
                    )


subUpdate :
    (a -> b -> ( b, Cmd a ))
    -> a
    -> b
    -> { model : b, cmd : Cmd a }
subUpdate subupdate submsg submodel =
    subupdate submsg submodel
        |> (\x -> { model = Tuple.first x, cmd = Tuple.second x })


prepInterlinearSave : Form.Interlinear.Model -> String -> Maybe M.Person -> Time.Posix -> Envelope
prepInterlinearSave int project me time =
    let
        meId : String
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
                , fragment = Nothing
                }
            , rev = Nothing
            , version = 1
            , comment = "No comment"
            , docversion = int.version
            , value = M.encoder (M.MyInterlinear interlinear)
            }
    in
    { command = "update-doc"
    , project = project
    , address = M.identifierToString (M.MyDocId <| M.InterlinearId int.id)
    , content =
        [ M.MyInterlinear interlinear
        , M.MyModification modification
        ]
            |> E.list M.encoder
    }


handleVista : Vista -> String -> String -> Model -> ( Model, Cmd Msg )
handleVista vista short full model =
    let
        vistas : Dict String Vista
        vistas =
            Dict.insert vista.identifier vista model.tabs.vistas

        loading : Set String
        loading =
            if vista.kind /= "interlinear" then
                Set.remove vista.project model.loading

            else
                model.loading

        tabs : Tab.Model
        tabs =
            model.tabs

        newmodel : Model
        newmodel =
            { model
                | tabs = { tabs | vistas = vistas }
                , loading = loading
            }
    in
    case getByVista vista.identifier model.tabs.ventanas of
        Nothing ->
            let
                key : String
                key =
                    getProjectKey vista.project model
                        |> Maybe.withDefault ""

                title : String
                title =
                    getProjectTitle vista.project model
                        |> Maybe.withDefault ""

                ventana : Ventana
                ventana =
                    { title = key ++ ": " ++ short
                    , fullTitle = title ++ ": " ++ full
                    , vista = vista.identifier
                    , params = { defVParams | length = 20 }
                    }
            in
            ( newmodel, sendMsg (Tab <| Tab.New ventana) )

        Just tp ->
            ( newmodel, sendMsg (Tab <| Tab.Focus tp) )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receivedGlobalConfig ReceivedGlobalConfig
        , receivedInterlinearIndex ReceivedInterlinearIndex
        , receivedAllDoc ReceivedAllDoc
        , receivedViewArea (\dimensions -> ViewArea dimensions |> Received)
        , newProject (\_ -> NewProject)
        , importOptions EditImporter
        , globalSettings ShowGlobalSettings
        , moveLeft_ (\_ -> Tab <| Tab.Move Left)
        , moveRight_ (\_ -> Tab <| Tab.Move Right)
        , moveUp_ (\_ -> Tab <| Tab.Move Up)
        , moveDown_ (\_ -> Tab <| Tab.Move Down)
        , closeTab_
            (\_ ->
                case model.tabs.focused of
                    Nothing ->
                        None

                    Just tp ->
                        Tab (Tab.Close tp)
            )
        , cloneTab_ (\_ -> Tab Tab.Clone)
        , toggleSidebar (\_ -> ToggleSidebar)
        ]


viewUnknownProgress : List (Html.Attribute Msg) -> List (Html.Html Msg) -> Html.Html Msg
viewUnknownProgress attrs children =
    Html.node "progress" attrs children


view : Model -> Html.Html Msg
view model =
    let
        tabtree : Dict Int (Dict Int (Set Int))
        tabtree =
            treeifyTabs <| Dict.keys model.tabs.ventanas
    in
    Html.main_ []
        [ if model.sideBar then
            Html.aside [ Attr.class "side" ]
                [ Html.a
                    [ Attr.href "#"
                    , Event.onClick ToggleSidebar
                    , Attr.title "Hide the sidebar"
                    ]
                    [ Html.text "Hide"
                    , Html.hr [] []
                    ]
                , Html.nav []
                    [ Html.h4 [] [ Html.text "Projects" ]
                    , viewProjects model
                    , if Dict.isEmpty model.tabs.visVentanas then
                        Html.text ""

                      else
                        Html.h4 [] [ Html.text "Open Tabs" ]
                    , viewTabList model.tabs.ventanas
                    ]
                ]

          else
            Html.aside [ Attr.class "hidden-side" ]
                [ Html.a
                    [ Attr.href "#"
                    , Event.onClick ToggleSidebar
                    , Attr.title "View the side bar"
                    ]
                    [ Html.text "View Side Bar" ]
                ]
        , if Dict.isEmpty tabtree then
            Html.div
                []
                [ if Set.isEmpty model.loading then
                    Html.h1 []
                        [ Html.text "Welcome!" ]

                  else
                    viewUnknownProgress [] []
                ]

          else
            Html.div
                [ Attr.id "content"
                , Attr.classList
                    [ ( "multiple-columns"
                      , columnCount (Dict.keys model.tabs.ventanas) > 1
                      )
                    , ( "multiple-rows"
                      , multipleRows (Dict.keys model.tabs.ventanas)
                      )
                    ]
                ]
                (Dict.map (viewColumn model) tabtree
                    |> Dict.values
                )
        ]


viewColumn : Model -> Int -> Dict Int (Set Int) -> Html.Html Msg
viewColumn model col rows =
    Html.div [ Attr.class "tab-column" ]
        (Dict.map (viewRow model col) rows
            |> Dict.values
        )


viewRow : Model -> Int -> Int -> Set Int -> Html.Html Msg
viewRow model col row tabs =
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
        ventana : Ventana
        ventana =
            Dict.get tp model.tabs.ventanas
                |> Maybe.withDefault
                    { title = "Error"
                    , fullTitle = "Error"
                    , vista = "Vista not found"
                    , params = defVParams
                    }

        focused : Bool
        focused =
            Just tp == model.tabs.focused

        visible : Bool
        visible =
            visMember tp model.tabs.visVentanas
    in
    Html.span []
        [ Html.button
            [ Event.onClick (Tab <| Tab.Select tp)
            , Event.onDoubleClick (Tab <| Tab.Clone)
            , Attr.id (pathToString tp)
            , Attr.classList
                [ ( "focused", focused )
                , ( "secondary", not focused && visible )
                , ( "secondary outline", not focused && not visible )
                , ( "tab-nav", True )
                ]
            , Attr.title ventana.fullTitle
            ]
            [ Html.text ventana.title ]
        , Html.button
            [ Event.onClick (Tab <| Tab.Close tp)
            , Attr.title "Close this tab"
            , Attr.classList
                [ ( "focused", focused )
                , ( "secondary", not focused && visible )
                , ( "secondary outline", not focused && not visible )
                , ( "close-button", True )
                ]
            ]
            [ Html.text "×" ]
        ]


viewTab : Model -> Tab.Path -> Html.Html Msg
viewTab model tp =
    Html.div
        [ Attr.classList
            [ ( "focused", Just tp == model.tabs.focused )
            , ( "hidden", not (visMember tp model.tabs.visVentanas) )
            , ( "tab-view", True )
            ]
        ]
        [ Html.div []
            [ Dict.get tp model.tabs.ventanas
                |> Maybe.andThen (\v -> Dict.get v.vista model.tabs.vistas)
                |> Maybe.map (viewVista model tp)
                |> Maybe.withDefault (Html.text "Failed!")
            ]
        ]


viewProjects : Model -> Html.Html Msg
viewProjects model =
    Html.ul [] <|
        List.map (viewProject model) model.gconfig.projects


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
            , Event.onClick <|
                MultiMsg
                    [ Tab <| Tab.Select tp
                    , Tab <| Tab.Goto tp
                    ]
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
        [ Html.text (p.title ++ ": " ++ p.key) ]


viewProject : Model -> ProjectInfo -> Html.Html Msg
viewProject model p =
    let
        pmodel : Form.Project.Model
        pmodel =
            Form.Project.fromProjectInfo p
    in
    Html.li []
        [ viewLoadingProject model p
        , Html.ul []
            [ Html.li []
                [ Html.a
                    [ Attr.href "#"
                    , Event.onClick <|
                        UserClick (EditProject pmodel)
                    , Attr.class "secondary"
                    ]
                    [ Html.text "Settings" ]
                ]
            , Html.li []
                [ Html.a
                    [ Attr.href "#"
                    , Event.onClick <|
                        UserClick (RequestInterlinearIndex p.identifier)
                    , Attr.class "secondary"
                    ]
                    [ Html.text "Gloss Index" ]
                ]
            , Html.li []
                [ Html.a
                    [ Attr.href "#"
                    , Event.onClick <|
                        UserClick (NewInterlinear p.identifier)
                    , Attr.class "secondary"
                    ]
                    [ Html.text "Gloss New Item" ]
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

        Content.ITV composite ->
            let
                editEvent : M.Interlinear -> Msg
                editEvent int =
                    case UUID.fromString vista.project of
                        Err _ ->
                            None

                        Ok uuid ->
                            EditInterlinear uuid int
                                
                params : Display.Composite.Params Msg
                params =
                    { composite = composite
                    , editEvent = editEvent
                    }
            in
            Display.Composite.view params

        Content.ITS ints ->
            let
                params : VentanaParams
                params =
                    Dict.get tp model.tabs.ventanas
                        |> Maybe.map .params
                        |> Maybe.withDefault { defVParams | length = 20 }

                ss : String
                ss =
                    params.searchString

                searched : List M.Interlinear
                searched =
                    List.filter
                        (\i ->
                            String.contains ss i.text
                                || String.contains ss i.ann.phonemic
                                || String.contains ss i.ann.glosses
                                || String.contains ss i.ann.breaks
                                || List.any
                                    (\t -> String.contains ss t.translation)
                                    (Dict.values i.translations)
                        )
                        ints

                intTotal : Int
                intTotal =
                    List.length searched

                is : List M.Interlinear
                is =
                    List.take params.length searched

                len : String
                len =
                    String.fromInt params.length

                viewEvent : UUID.UUID -> Msg
                viewEvent  identifier =
                    [ "interlinear/", UUID.toString identifier ]
                        |> String.concat
                        |> RequestAllDocId vista.project
                        |> UserClick

                editEvent : M.Interlinear -> Msg
                editEvent interlinear =
                    UserClick (EditInterlinear interlinear.id interlinear)

                displayParams : Display.InterlinearListing.Params Msg
                displayParams =
                    { interlinears = is
                    , viewEvent = viewEvent
                    , editEvent = editEvent
                    }
            in
            Html.div []
                [ Html.div [ Attr.class "filters" ]
                    [ Html.label []
                        [ Html.text <|
                            if params.length <= intTotal then
                                let
                                    -- For displaying
                                    total : String
                                    total =
                                        intTotal |> String.fromInt
                                in
                                "Show (" ++ len ++ " of " ++ total ++ ")"

                            else
                                "Showing all items."
                        , Html.input
                            ([ Attr.type_ "text"
                             , Attr.placeholder len
                             , Event.onInput
                                (\s ->
                                    Tab.Change Tab.Length tp s
                                        |> Tab
                                )
                             ]
                                ++ (if params.length > 0 then
                                        [ Attr.value len ]

                                    else
                                        []
                                   )
                            )
                            []
                        , Html.input
                            [ Attr.type_ "text"
                            , Attr.value ss
                            , Attr.placeholder "Search"
                            , Attr.attribute "aria-label" "Search"
                            , Event.onInput
                                (\s ->
                                    Tab.Change Tab.Search tp s
                                        |> Tab
                                )
                            ]
                            []
                        ]
                    ]
                , Display.InterlinearListing.view displayParams
                ]


getProjectTitle : String -> Model -> Maybe String
getProjectTitle projid model =
    let
        projuuid : Maybe UUID.UUID
        projuuid =
            UUID.fromString projid |> Result.toMaybe

        projects : List ProjectInfo
        projects =
            model.gconfig.projects
    in
    LE.find (\x -> Just x.identifier == projuuid) projects
        |> Maybe.map .title


getProjectKey : String -> Model -> Maybe String
getProjectKey projid model =
    let
        projuuid : Maybe UUID.UUID
        projuuid =
            UUID.fromString projid |> Result.toMaybe

        projects : List ProjectInfo
        projects =
            model.gconfig.projects
    in
    LE.find (\x -> Just x.identifier == projuuid) projects
        |> Maybe.map .key


reduceDoc : Envelope -> Result D.Error M.Composite
reduceDoc env =
    let
        content : Result D.Error (List M.Value)
        content =
            env.content |> D.decodeValue M.listDecoder
    in
    case content of
        Ok content_ ->
            let
                initial : M.Composite
                initial =
                    M.Composite Nothing [] [] [] []
            in
            Ok <| List.foldl M.compositeBuilder initial content_

        Err e ->
            Err e



{- PORTS -}


port send : E.Value -> Cmd msg


{-| The window title changes depending on the focused tab. This sends
the signal to the backend to do so.
-}



-- port setWindowTitle : String -> Cmd msg


port requestInterlinearIndex : String -> Cmd msg


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


{-| Received information about client area size.
-}
port receivedViewArea : (E.Value -> msg) -> Sub msg


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


port toggleSidebar : (() -> msg) -> Sub msg
