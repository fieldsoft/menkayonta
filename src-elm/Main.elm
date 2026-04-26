port module Main exposing (Dimensions, Flags, Model, Msg, main)

import AssocList as AL
import Browser
import Config exposing (GlobalConfig, ProjectInfo)
import Content exposing (Content(..))
import Dict exposing (Dict)
import Display.Composite
import Display.InterlinearListing
import Display.KeyedInterlinearListing
import Display.Meta
import Display.Note
import Display.PersonListing
import Display.SequenceListing
import Form.Global
import Form.Importer
import Form.Interlinear
import Form.Project
import Form.Sequence
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import Json.Decode as D
import Json.Encode as E
import Keyed as K
import List.Extra as LE
import Menkayonta as M
import Meta
import Msg exposing (EditType(..), ReceiveType(..), RequestType(..))
import Random
import Result.Extra as RE
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
import Unicode
import Url


type alias Model =
    { gconfig : GlobalConfig
    , me : Maybe M.Person
    , tabs : Tab.Model
    , error : String
    , loading : Set String
    , seeds : UUID.Seeds
    , sideBar : Bool
    , viewArea : Dimensions
    , status : AL.Dict ProjectId (Dict String Int)
    }


type alias ProjectId =
    UUID.UUID


type Msg
    = GF Form.Global.Msg
    | IM Form.Importer.Msg
    | ITE UUID.UUID Form.Interlinear.Msg
    | SQE UUID.UUID Form.Sequence.Msg
    | EditImporter String
    | MultiMsg (List Msg)
    | Ms Msg.Msg
    | PR ProjectId Form.Project.Msg
    | ShowGlobalSettings E.Value
    | Stamp (Time.Posix -> Envelope) (E.Value -> Cmd Msg) Time.Posix
    | Tab Tab.Msg
    | ToggleSidebar


{-| Inject a message into `Cmd`
-}
sendMsg : Msg -> Cmd Msg
sendMsg msg =
    Task.succeed msg
        |> Task.perform identity


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
    , project : ProjectId
    , address : String
    , content : E.Value
    }


envelopeDecoder : D.Decoder Envelope
envelopeDecoder =
    D.map4 Envelope
        (D.field "command" D.string)
        (D.field "project" UUID.jsonDecoder)
        (D.field "address" D.string)
        (D.field "content" D.value)


envelopeEncoder : Envelope -> E.Value
envelopeEncoder env =
    E.object
        [ ( "command", E.string env.command )
        , ( "project", UUID.toValue env.project )
        , ( "address", E.string env.address )
        , ( "content", env.content )
        ]


globalSettingsVista : Vista
globalSettingsVista =
    { project = "global"
    , path = "/"
    , identifier = "global-settings"
    , content = Content.GF Form.Global.initData
    }


importOptionsVista : Vista
importOptionsVista =
    { project = "global"
    , path = "/"
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
      , status = AL.empty
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
        Ms Msg.None ->
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

        Ms (Msg.Status envelopeJson) ->
            handleStatus model envelopeJson

        Ms (Msg.ChangeNote vistaid str) ->
            handleNoteChange model vistaid str

        Ms Msg.EditToggle ->
            case model.tabs.focused of
                Nothing ->
                    ( model, Cmd.none )

                Just tp ->
                    ( model, Tab.Change Tab.Edit tp "" |> Tab |> sendMsg )

        Ms (Msg.UserClick clickMsg) ->
            let
                subModel : Tab.Model
                subModel =
                    Tab.update Tab.Unlock model.tabs
                        |> Tuple.first
            in
            ( { model | tabs = subModel }, sendMsg (Ms clickMsg) )

        ToggleSidebar ->
            ( { model | sideBar = not model.sideBar }
            , Cmd.none
            )

        Tab subMsg ->
            let
                t : { model : Tab.Model, cmd : Cmd Tab.Msg }
                t =
                    subUpdate Tab.update subMsg model.tabs
            in
            ( { model | tabs = t.model }
            , Cmd.map Tab t.cmd
            )

        Ms (Msg.Received rt) ->
            handleReceived model rt

        Ms (Msg.SaveTag tagfield) ->
            let
                payload : E.Value
                payload =
                    tagfield.docids
                        |> List.map
                            (\docid ->
                                { id =
                                    { kind = tagfield.kind
                                    , docid = docid
                                    , fragment = Nothing
                                    }
                                , rev = Nothing
                                , version = 1
                                }
                                    |> M.MyTag
                            )
                        |> E.list M.encoder

                path : String
                path =
                    Tab.getFocusedVista model.tabs
                        |> Maybe.map .path
                        |> Maybe.withDefault ""

                envelope : E.Value
                envelope =
                    envelopeEncoder
                        { command = "bulk-write"
                        , project = tagfield.project
                        , address = path
                        , content = payload
                        }
            in
            ( model
            , Cmd.batch
                [ sendMsg (Ms (Msg.ChangeTag Nothing))
                , send envelope
                ]
            )

        Ms (Msg.SaveNote note) ->
            case Tab.getFocusedVista model.tabs of
                Nothing ->
                    ( model, Cmd.none )

                Just vista ->
                    case UUID.fromString vista.project of
                        Err _ ->
                            ( model, Cmd.none )

                        Ok project ->
                            let
                                payload : E.Value
                                payload =
                                    [ M.MyNote note ]
                                        |> E.list M.encoder

                                envelope : E.Value
                                envelope =
                                    envelopeEncoder
                                        { command = "bulk-write"
                                        , project = project
                                        , address = vista.path
                                        , content = payload
                                        }
                            in
                            ( model
                            , send envelope
                            )

        Ms (Msg.SaveProperty propertyfield) ->
            let
                payload : E.Value
                payload =
                    propertyfield.docids
                        |> List.map
                            (\docid ->
                                { id =
                                    { kind = propertyfield.kind
                                    , value = propertyfield.value
                                    , docid = docid
                                    , fragment = Nothing
                                    }
                                , rev = Nothing
                                , version = 1
                                }
                                    |> M.MyProperty
                            )
                        |> E.list M.encoder

                path : String
                path =
                    Tab.getFocusedVista model.tabs
                        |> Maybe.map .path
                        |> Maybe.withDefault ""

                envelope : E.Value
                envelope =
                    envelopeEncoder
                        { command = "bulk-write"
                        , project = propertyfield.project
                        , address = path
                        , content = payload
                        }
            in
            ( model
            , Cmd.batch
                [ sendMsg (Ms (Msg.ChangeProperty Nothing))
                , send envelope
                ]
            )

        Ms (Msg.ChangeTag tagfield) ->
            case model.tabs.focused of
                Nothing ->
                    ( model, Cmd.none )

                Just tp ->
                    case Dict.get tp model.tabs.ventanas of
                        Nothing ->
                            ( model, Cmd.none )

                        Just ventana ->
                            let
                                params : Tab.VentanaParams
                                params =
                                    ventana.params

                                meta : Meta.Dialog
                                meta =
                                    params.meta

                                params_ : Tab.VentanaParams
                                params_ =
                                    { params
                                        | meta =
                                            { meta
                                                | tag =
                                                    tagfield
                                            }
                                    }

                                ventana_ : Tab.Ventana
                                ventana_ =
                                    { ventana | params = params_ }

                                ventanas : Tab.Ventanas
                                ventanas =
                                    Dict.insert tp
                                        ventana_
                                        model.tabs.ventanas

                                tabs : Tab.Model
                                tabs =
                                    model.tabs
                            in
                            ( { model
                                | tabs = { tabs | ventanas = ventanas }
                              }
                            , Cmd.none
                            )

        Ms (Msg.ChangeProperty propertyfield) ->
            case model.tabs.focused of
                Nothing ->
                    ( model, Cmd.none )

                Just tp ->
                    case Dict.get tp model.tabs.ventanas of
                        Nothing ->
                            ( model, Cmd.none )

                        Just ventana ->
                            let
                                params : Tab.VentanaParams
                                params =
                                    ventana.params

                                meta : Meta.Dialog
                                meta =
                                    params.meta

                                params_ : Tab.VentanaParams
                                params_ =
                                    { params
                                        | meta =
                                            { meta
                                                | property =
                                                    propertyfield
                                            }
                                    }

                                ventana_ : Tab.Ventana
                                ventana_ =
                                    { ventana | params = params_ }

                                ventanas : Tab.Ventanas
                                ventanas =
                                    Dict.insert tp
                                        ventana_
                                        model.tabs.ventanas

                                tabs : Tab.Model
                                tabs =
                                    model.tabs
                            in
                            ( { model
                                | tabs = { tabs | ventanas = ventanas }
                              }
                            , Cmd.none
                            )

        ITE id subMsg ->
            let
                id_ : String
                id_ =
                    "FORM::" ++ UUID.toString id

                maybeData : Maybe ( Vista, Tab.Path, ProjectId )
                maybeData =
                    case
                        ( Dict.get id_ model.tabs.vistas
                        , getByVista id_ model.tabs.ventanas
                        )
                    of
                        ( Just v, Just t ) ->
                            case UUID.fromString v.project of
                                Err _ ->
                                    Nothing

                                Ok uuid ->
                                    Just ( v, t, uuid )

                        _ ->
                            Nothing
            in
            case maybeData of
                Nothing ->
                    -- Something is wrong. Ignore the message.
                    ( model, Cmd.none )

                Just ( vista_, tp, project ) ->
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
                                        project
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

        SQE id subMsg ->
            let
                id_ : String
                id_ =
                    "FORM::" ++ UUID.toString id

                maybeData : Maybe ( Vista, Tab.Path, ProjectId )
                maybeData =
                    case
                        ( Dict.get id_ model.tabs.vistas
                        , getByVista id_ model.tabs.ventanas
                        )
                    of
                        ( Just v, Just t ) ->
                            case UUID.fromString v.project of
                                Err _ ->
                                    Nothing

                                Ok uuid ->
                                    Just ( v, t, uuid )

                        _ ->
                            Nothing
            in
            case maybeData of
                Nothing ->
                    -- Something is wrong. Ignore the message.
                    ( model, Cmd.none )

                Just ( vista_, tp, project ) ->
                    let
                        oldModel : Form.Sequence.Model
                        oldModel =
                            case vista_.content of
                                Content.SQE model_ ->
                                    model_

                                _ ->
                                    Form.Sequence.initData id

                        s :
                            { model : Form.Sequence.Model
                            , cmd : Cmd Form.Sequence.Msg
                            }
                        s =
                            subUpdate
                                Form.Sequence.update
                                subMsg
                                oldModel

                        vista : Vista
                        vista =
                            { vista_ | content = Content.SQE s.model }

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
                        Form.Sequence.Cancel ->
                            ( nmodel
                            , Cmd.batch
                                [ sendMsg (Tab (Tab.Close tp))
                                , Cmd.map (SQE id) s.cmd
                                ]
                            )

                        Form.Sequence.Save ->
                            let
                                envelopePart : Maybe (Time.Posix -> Envelope)
                                envelopePart =
                                    prepSequenceSave
                                        s.model
                                        project
                                        model.me
                            in
                            case envelopePart of
                                Just ep ->
                                    ( nmodel
                                    , Cmd.batch
                                        [ sendMsg (Tab (Tab.Close tp))
                                        , Task.perform
                                            (Stamp ep send)
                                            Time.now
                                        , Cmd.map (SQE id) s.cmd
                                        ]
                                    )

                                Nothing ->
                                    ( nmodel, Cmd.map (SQE id) s.cmd )

                        _ ->
                            ( nmodel, Cmd.map (SQE id) s.cmd )

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
                                    , path = UUID.toString id
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

        Ms (Msg.Request project rt) ->
            handleRequest model project rt

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
                    , path = "/"
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
                    , path = "/"
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

        Ms Msg.NewProject ->
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
                    , path = id
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

        Ms (Msg.EditProject project) ->
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
                    , path = id
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

        Ms (Msg.Edit editType) ->
            handleEditType model editType


handleEditType : Model -> EditType -> ( Model, Cmd Msg )
handleEditType model et =
    case et of
        EPerson project person ->
            ( model, Cmd.none )

        NPerson project ->
            ( model, Cmd.none )

        EInterlinear project int ->
            handleEditInterlinear model project int

        NInterlinear project ->
            handleNewInterlinear model project

        ESequence project seq ->
            handleEditSequence model project seq

        NSequence project ->
            handleNewSequence model project


handleRequest : Model -> UUID.UUID -> RequestType -> ( Model, Cmd Msg )
handleRequest model project rt =
    case rt of
        OReversal (Just str) ->
            handleOReversal model project str

        OReversal Nothing ->
            ( model, Cmd.none )

        OInterlinearListing ->
            handleOInterlinearListing model project

        OSequenceListing ->
            handleOSequenceListing model project

        OPersonListing ->
            handleOPersonListing model project

        OSequence str ->
            handleOSequence model project str

        OComposite str ->
            handleOComposite model project str

        ODelete (Just rev) str ->
            handleODelete model project rev str

        ODelete Nothing _ ->
            ( model, Cmd.none )

        ONoteFor desc ->
            handleONoteFor model project desc

        ONote str ->
            handleONote model project str


handleReceived : Model -> ReceiveType -> ( Model, Cmd Msg )
handleReceived model rt =
    case rt of
        IReload envelopeJson ->
            handleIReload model envelopeJson

        INoteFor envelopeJson ->
            handleINoteFor model envelopeJson

        INote envelopeJson ->
            handleINote model envelopeJson

        IViewArea jsonValue ->
            handleIViewArea model jsonValue

        IGlobalConfig gcJson ->
            handleIGlobalConfig model gcJson

        IInterlinearListing envelopeJson ->
            handleIInterlinearListing model envelopeJson

        ISequenceListing envelopeJson ->
            handleISequenceListing model envelopeJson

        ISequence envelopeJson ->
            handleISequence model envelopeJson

        IPersonListing envelopeJson ->
            handleIPersonListing model envelopeJson

        IReversal envelopeJson ->
            handleIReversal model envelopeJson

        IComposite envelopeJson ->
            handleIComposite model envelopeJson


subUpdate : (a -> b -> ( b, Cmd a )) -> a -> b -> { model : b, cmd : Cmd a }
subUpdate subupdate submsg submodel =
    subupdate submsg submodel
        |> (\x -> { model = Tuple.first x, cmd = Tuple.second x })


prepInterlinearSave : Form.Interlinear.Model -> ProjectId -> Maybe M.Person -> Time.Posix -> Envelope
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
                , alternate = int.annotations.alternate.value
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


prepSequenceSave :
    Form.Sequence.Model
    -> ProjectId
    -> Maybe M.Person
    -> Maybe (Time.Posix -> Envelope)
prepSequenceSave seq project me =
    let
        kindVal : Maybe M.SequenceKind
        kindVal =
            case seq.kind.value of
                "integer" ->
                    Just M.Integer

                "string" ->
                    Just M.StringKey

                _ ->
                    Nothing

        decodeValue : String -> Result UUID.Error UUID.UUID
        decodeValue v =
            case String.split "/" v of
                "interlinear" :: uustr :: [] ->
                    UUID.fromString uustr

                _ ->
                    UUID.fromString ""

        itemVals : Result UUID.Error (List UUID.UUID)
        itemVals =
            seq.items
                |> List.map .value
                |> List.map .value
                |> List.map decodeValue
                |> RE.combine
    in
    case ( me, kindVal, itemVals ) of
        ( Just me_, Just kind, Ok values ) ->
            let
                sequence : M.Sequence
                sequence =
                    { id = seq.id
                    , rev = seq.rev
                    , version = seq.version
                    , title = seq.title.value
                    , description = seq.description.value
                    , kind = kind
                    , items =
                        seq.items
                            |> List.filter (\x -> not x.deleted)
                            |> List.map2
                                (\uuid item ->
                                    { key =
                                        item.key.value
                                    , value =
                                        uuid
                                    }
                                )
                                values
                    }

                modification : Time.Posix -> M.Modification
                modification =
                    \time ->
                        { id =
                            { kind = "update"
                            , docid = M.SequenceId seq.id
                            , time = time
                            , person = M.PersonId me_.id
                            , fragment = Nothing
                            }
                        , rev = Nothing
                        , version = 1
                        , comment = "No comment"
                        , docversion = seq.version
                        , value = M.encoder (M.MySequence sequence)
                        }
            in
            Just
                (\time ->
                    { command = "update-doc"
                    , project = project
                    , address =
                        M.identifierToString
                            (M.MyDocId <| M.SequenceId seq.id)
                    , content =
                        [ M.MySequence sequence
                        , M.MyModification (modification time)
                        ]
                            |> E.list M.encoder
                    }
                )

        _ ->
            Nothing


handleReceivedNote : UUID.UUID -> Model -> M.GenericDesc -> M.Note -> ( Model, Cmd Msg )
handleReceivedNote project model gd note =
    let
        projectStr : String
        projectStr =
            UUID.toString project

        full : String
        full =
            String.concat [ "Note: ", gd.title ]

        short : String
        short =
            if String.length gd.title > 7 then
                String.concat
                    [ "Note: "
                    , String.left 7 gd.title
                    , "..."
                    ]

            else
                full

        content : Content
        content =
            Content.NTV
                { title = gd.title
                , description = gd.description
                , note = note
                , original = note.note
                }

        idstr : String
        idstr =
            M.identifierToString (M.MyNoteId note.id)

        vista : Tab.Vista
        vista =
            { project = projectStr
            , path = idstr
            , identifier = "NOTE::" ++ idstr
            , content = content
            }
    in
    handleVista vista short full model


handleVista : Vista -> String -> String -> Model -> ( Model, Cmd Msg )
handleVista vista short full model =
    let
        vistas : Dict String Vista
        vistas =
            Dict.insert vista.identifier vista model.tabs.vistas

        loading : Set String
        loading =
            if String.startsWith "interlinear" vista.path then
                Set.remove vista.project model.loading

            else if String.startsWith "sequence" vista.path then
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
subscriptions _ =
    let
        rcvd : (E.Value -> Msg.ReceiveType) -> (E.Value -> Msg)
        rcvd t =
            \v -> t v |> Msg.Received |> Ms

        dtp : D.Decoder Tab.Path
        dtp =
            D.map3 (\c r t -> ( c, ( r, t ) ))
                (D.field "column" D.int)
                (D.field "row" D.int)
                (D.field "tab" D.int)

        move : Direction -> E.Value -> Msg
        move d value =
            tabAct (Tab.Move d) value

        tabAct : (Tab.Path -> Tab.Msg) -> E.Value -> Msg
        tabAct a value =
            case D.decodeValue dtp value of
                Err _ ->
                    Ms Msg.None

                Ok tp ->
                    a tp |> Tab

        ms : (E.Value -> Msg.Msg) -> E.Value -> Msg
        ms m v =
            Ms (m v)
    in
    Sub.batch
        [ receivedGlobalConfig <| rcvd IGlobalConfig
        , receivedInterlinearListing <| rcvd IInterlinearListing
        , receivedSequenceListing <| rcvd ISequenceListing
        , receivedSequence <| rcvd ISequence
        , receivedPersonListing <| rcvd IPersonListing
        , receivedInterlinearReversals <| rcvd IReversal
        , receivedComposite <| rcvd IComposite
        , receivedViewArea <| rcvd IViewArea
        , receivedReloadRequest <| rcvd IReload
        , receivedNote <| rcvd INote
        , receivedNoteFor <| rcvd INoteFor
        , newProject <| \_ -> Ms Msg.NewProject
        , importOptions EditImporter
        , globalSettings ShowGlobalSettings
        , moveLeft <| move Left
        , moveRight <| move Right
        , moveUp <| move Up
        , moveDown <| move Down
        , closeTab <| tabAct Tab.Close
        , cloneTab <| tabAct Tab.Clone
        , toggleSidebar (\_ -> ToggleSidebar)
        , status <| ms Msg.Status
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
        [ Html.a
            [ Event.onClick (Tab <| Tab.Select tp)
            , Event.onDoubleClick (Tab <| Tab.Clone tp)
            , Attr.id (pathToString tp)
            , Attr.classList
                [ ( "focused", focused )
                , ( "secondary", not focused && visible )
                , ( "secondary outline", not focused && not visible )
                , ( "tab-nav", True )
                ]
            , Attr.title ventana.fullTitle
            , Attr.attribute "role" "button"
            , Attr.href <| "#tabnav#" ++ pathToString tp
            ]
            [ Html.text ventana.title ]
        , Html.a
            [ Event.onClick (Tab <| Tab.Close tp)
            , Attr.title "Close this tab"
            , Attr.classList
                [ ( "focused", focused )
                , ( "secondary", not focused && visible )
                , ( "secondary outline", not focused && not visible )
                , ( "close-button", True )
                ]
            , Attr.attribute "role" "button"
            , Attr.href "#"
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
        , Event.onMouseEnter <| Tab <| Tab.Select tp
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
    Html.summary
        [ Attr.attribute "aria-busy"
            (if Set.member (UUID.toString p.identifier) model.loading then
                "true"

             else
                "false"
            )
        ]
        [ Html.span [] [ Html.text p.title ]
        , Html.text ": "
        , Html.b [] [ Html.text p.key ]
        , case AL.get p.identifier model.status of
            Nothing ->
                Html.text ""

            Just stat ->
                Html.span [] <|
                    List.map
                        (\( k, v ) ->
                            case k of
                                "view_indexing" ->
                                    if v > 0 then
                                        Html.text " I "

                                    else
                                        Html.text ""

                                "replication" ->
                                    Html.text " R "

                                _ ->
                                    Html.text ""
                        )
                        (Dict.toList stat)
        ]


viewProject : Model -> ProjectInfo -> Html.Html Msg
viewProject model p =
    let
        pmodel : Form.Project.Model
        pmodel =
            Form.Project.fromProjectInfo p
    in
    Html.li []
        [ Html.details []
            [ viewLoadingProject model p
            , Html.ul []
                [ Html.li []
                    [ Html.a
                        [ Attr.href "#"
                        , Msg.EditProject pmodel
                            |> Msg.UserClick
                            |> Ms
                            |> Event.onClick
                        , Attr.class "secondary"
                        ]
                        [ Html.text "Settings" ]
                    ]
                , Html.li []
                    [ Html.a
                        [ Attr.href "#"
                        , Msg.Request p.identifier OInterlinearListing
                            |> Msg.UserClick
                            |> Ms
                            |> Event.onClick
                        , Attr.class "secondary"
                        ]
                        [ Html.text "Interlinear Glosses" ]
                    ]
                , Html.li []
                    [ Html.a
                        [ Attr.href "#"
                        , Msg.Request p.identifier OSequenceListing
                            |> Msg.UserClick
                            |> Ms
                            |> Event.onClick
                        , Attr.class "secondary"
                        ]
                        [ Html.text "Sequences" ]
                    ]
                , Html.li []
                    [ Html.a
                        [ Attr.href "#"
                        , Msg.Request p.identifier OPersonListing
                            |> Msg.UserClick
                            |> Ms
                            |> Event.onClick
                        , Attr.class "secondary"
                        ]
                        [ Html.text "People" ]
                    ]
                , Html.li []
                    [ Html.a
                        [ Attr.href "#"
                        , NInterlinear p.identifier
                            |> Msg.Edit
                            |> Msg.UserClick
                            |> Ms
                            |> Event.onClick
                        , Attr.class "secondary"
                        ]
                        [ Html.text "New Interlinear Gloss" ]
                    ]
                , Html.li []
                    [ Html.a
                        [ Attr.href "#"
                        , NSequence p.identifier
                            |> Msg.Edit
                            |> Msg.UserClick
                            |> Ms
                            |> Event.onClick
                        , Attr.class "secondary"
                        ]
                        [ Html.text "New Sequence" ]
                    ]
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

        Content.SQE smodel ->
            Form.Sequence.view smodel |> Html.map (SQE smodel.id)

        Content.PR pmodel ->
            Form.Project.view pmodel |> Html.map (PR pmodel.identifier)

        Content.NTV note ->
            ntvContent model tp vista note

        Content.ITV composite ->
            itvContent model tp vista composite

        Content.SQV composite ->
            sqvContent model tp vista composite

        Content.PLV composite ->
            plvContent model tp vista composite

        Content.ITS ints ->
            itsContent model tp vista ints

        Content.SQS seqs ->
            sqsContent model tp vista seqs

        Content.PLS people ->
            plsContent model tp vista people

        Content.PGS pages ->
            pgsContent model tp vista pages

        Content.SDV seqData ->
            sdvContent model tp vista seqData


ntvContent : Model -> Tab.Path -> Vista -> Display.Note.Model -> Html Msg
ntvContent model tp vista note =
    let
        edit : Bool
        edit =
            Dict.get tp model.tabs.ventanas
                |> Maybe.map .params
                |> Maybe.map .edit
                |> Maybe.withDefault False
    in
    if edit then
        Display.Note.edit note |> Html.map Ms

    else
        Display.Note.view note |> Html.map Ms


itvContent : Model -> Tab.Path -> Vista -> M.Composite -> Html Msg
itvContent model tp vista composite =
    case
        UUID.fromString vista.project
            |> Result.toMaybe
    of
        Just project ->
            let
                cmodel : Display.Composite.Model
                cmodel =
                    { composite = composite
                    , project = project
                    }

                display : Html.Html Msg
                display =
                    Display.Composite.view cmodel
                        |> Html.map Ms

                tagfield : Html.Html Msg
                tagfield =
                    Dict.get tp model.tabs.ventanas
                        |> Maybe.map .params
                        |> Maybe.map .meta
                        |> Maybe.andThen .tag
                        |> Maybe.map Display.Meta.tagField
                        |> Maybe.map (Html.map Ms)
                        |> Maybe.withDefault (Html.text "")

                propfield : Html.Html Msg
                propfield =
                    Dict.get tp model.tabs.ventanas
                        |> Maybe.map .params
                        |> Maybe.map .meta
                        |> Maybe.andThen .property
                        |> Maybe.map Display.Meta.propertyField
                        |> Maybe.map (Html.map Ms)
                        |> Maybe.withDefault (Html.text "")
            in
            Html.div []
                [ tagfield
                , propfield
                , display
                ]

        _ ->
            Html.text "Invalid project or non-interlinear"


plvContent : Model -> Tab.Path -> Vista -> M.Composite -> Html Msg
plvContent model tp vista composite =
    case
        UUID.fromString vista.project
            |> Result.toMaybe
    of
        Just project ->
            let
                cmodel : Display.Composite.Model
                cmodel =
                    { composite = composite
                    , project = project
                    }

                display : Html.Html Msg
                display =
                    Display.Composite.view cmodel
                        |> Html.map Ms

                tagfield : Html.Html Msg
                tagfield =
                    Dict.get tp model.tabs.ventanas
                        |> Maybe.map .params
                        |> Maybe.map .meta
                        |> Maybe.andThen .tag
                        |> Maybe.map Display.Meta.tagField
                        |> Maybe.map (Html.map Ms)
                        |> Maybe.withDefault (Html.text "")

                propfield : Html.Html Msg
                propfield =
                    Dict.get tp model.tabs.ventanas
                        |> Maybe.map .params
                        |> Maybe.map .meta
                        |> Maybe.andThen .property
                        |> Maybe.map Display.Meta.propertyField
                        |> Maybe.map (Html.map Ms)
                        |> Maybe.withDefault (Html.text "")
            in
            Html.div []
                [ tagfield
                , propfield
                , display
                ]

        _ ->
            Html.text "Invalid project or non-interlinear"


sqvContent : Model -> Tab.Path -> Vista -> M.Composite -> Html Msg
sqvContent model tp vista composite =
    case
        UUID.fromString vista.project
            |> Result.toMaybe
    of
        Just project ->
            let
                cmodel : Display.Composite.Model
                cmodel =
                    { composite = composite
                    , project = project
                    }

                display : Html.Html Msg
                display =
                    Display.Composite.view cmodel
                        |> Html.map Ms

                tagfield : Html.Html Msg
                tagfield =
                    Dict.get tp model.tabs.ventanas
                        |> Maybe.map .params
                        |> Maybe.map .meta
                        |> Maybe.andThen .tag
                        |> Maybe.map Display.Meta.tagField
                        |> Maybe.map (Html.map Ms)
                        |> Maybe.withDefault (Html.text "")

                propfield : Html.Html Msg
                propfield =
                    Dict.get tp model.tabs.ventanas
                        |> Maybe.map .params
                        |> Maybe.map .meta
                        |> Maybe.andThen .property
                        |> Maybe.map Display.Meta.propertyField
                        |> Maybe.map (Html.map Ms)
                        |> Maybe.withDefault (Html.text "")
            in
            Html.div []
                [ tagfield
                , propfield
                , display
                ]

        _ ->
            Html.text "Invalid project or non-sequence"


itsContent : Model -> Tab.Path -> Vista -> List M.Interlinear -> Html Msg
itsContent model tp vista ints =
    case
        UUID.fromString vista.project
            |> Result.toMaybe
    of
        Nothing ->
            Html.text "Missing project information"

        Just project ->
            let
                params : VentanaParams
                params =
                    Dict.get tp model.tabs.ventanas
                        |> Maybe.map .params
                        |> Maybe.withDefault
                            { defVParams | length = 20 }

                ss : String
                ss =
                    params.searchString

                searched : List M.Interlinear
                searched =
                    let
                        strings :
                            M.Interlinear
                            -> List ( String, String )
                        strings int =
                            Dict.foldl
                                (\_ t acc ->
                                    ( "translations.judgment"
                                    , t.judgment
                                    )
                                        :: ( "translation"
                                           , t.translation
                                           )
                                        :: acc
                                )
                                []
                                int.translations
                                |> List.append
                                    [ ( "text", int.text )
                                    , ( "judgment", int.ann.judgment )
                                    , ( "phonemic", int.ann.phonemic )
                                    , ( "glosses", int.ann.glosses )
                                    , ( "breaks", int.ann.breaks )
                                    ]
                    in
                    List.filter (\i -> search ss (strings i)) ints

                intTotal : Int
                intTotal =
                    List.length searched

                is : List M.Interlinear
                is =
                    List.take params.length searched

                len : String
                len =
                    String.fromInt params.length

                imodel : Display.InterlinearListing.Model
                imodel =
                    { interlinears = is
                    , project = project
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
                , Display.InterlinearListing.view imodel |> Html.map Ms
                ]


pgsContent : Model -> Tab.Path -> Vista -> List M.Page -> Html Msg
pgsContent _ _ _ _ =
    Html.text "Not implemented"
        
sqsContent : Model -> Tab.Path -> Vista -> List M.Sequence -> Html Msg
sqsContent model tp vista seqs =
    case
        UUID.fromString vista.project
            |> Result.toMaybe
    of
        Just project ->
            let
                params : VentanaParams
                params =
                    Dict.get tp model.tabs.ventanas
                        |> Maybe.map .params
                        |> Maybe.withDefault
                            { defVParams | length = 20 }

                ss : String
                ss =
                    params.searchString

                searched : List M.Sequence
                searched =
                    let
                        strings :
                            M.Sequence
                            -> List ( String, String )
                        strings seq =
                            [ ( "title"
                              , seq.title
                              )
                            , ( "description"
                              , seq.description
                              )
                            ]
                    in
                    List.filter (\i -> search ss (strings i)) seqs

                intTotal : Int
                intTotal =
                    List.length searched

                is : List M.Sequence
                is =
                    List.take params.length searched

                len : String
                len =
                    String.fromInt params.length

                imodel : Display.SequenceListing.Model
                imodel =
                    { sequences = is
                    , project = project
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
                , Display.SequenceListing.view imodel |> Html.map Ms
                ]

        Nothing ->
            Html.text "Missing project information"


plsContent : Model -> Tab.Path -> Vista -> List M.Person -> Html Msg
plsContent model tp vista people =
    case
        UUID.fromString vista.project
            |> Result.toMaybe
    of
        Nothing ->
            Html.text "Missing project information"

        Just project ->
            let
                params : VentanaParams
                params =
                    Dict.get tp model.tabs.ventanas
                        |> Maybe.map .params
                        |> Maybe.withDefault { defVParams | length = 20 }

                ss : String
                ss =
                    params.searchString

                searched : List M.Person
                searched =
                    let
                        strings : M.Person -> List ( String, String )
                        strings person =
                            Dict.foldl
                                (\_ n acc ->
                                    ( "name", n ) :: acc
                                )
                                []
                                person.names
                                |> List.append
                                    [ ( "id", person.id ) ]
                    in
                    List.filter (\p -> search ss (strings p)) people

                intTotal : Int
                intTotal =
                    List.length searched

                ps : List M.Person
                ps =
                    List.take params.length searched

                len : String
                len =
                    String.fromInt params.length

                pmodel : Display.PersonListing.Model
                pmodel =
                    { people = ps
                    , project = project
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
                , Display.PersonListing.view pmodel |> Html.map Ms
                ]


sdvContent : Model -> Tab.Path -> Vista -> K.SeqData -> Html Msg
sdvContent model tp vista seqd =
    case
        UUID.fromString vista.project
            |> Result.toMaybe
    of
        Just project ->
            let
                params : VentanaParams
                params =
                    Dict.get tp model.tabs.ventanas
                        |> Maybe.map .params
                        |> Maybe.withDefault
                            { defVParams | length = 20 }

                ss : String
                ss =
                    params.searchString

                searched : List ( String, M.Interlinear )
                searched =
                    let
                        strings : String -> M.Interlinear -> List ( String, String )
                        strings key int =
                            Dict.foldl
                                (\_ t acc ->
                                    ( "translations.judgment"
                                    , t.judgment
                                    )
                                        :: ( "translation"
                                           , t.translation
                                           )
                                        :: acc
                                )
                                []
                                int.translations
                                |> List.append
                                    [ ( "key", key )
                                    , ( "text", int.text )
                                    , ( "judgment", int.ann.judgment )
                                    , ( "phonemic", int.ann.phonemic )
                                    , ( "glosses", int.ann.glosses )
                                    , ( "breaks", int.ann.breaks )
                                    ]

                        filterByValue : List ( String, M.Interlinear ) -> List ( String, M.Interlinear )
                        filterByValue =
                            List.filter (\( k, v ) -> search ss (strings k v))
                    in
                    case seqd.docs of
                        K.IKey docs ->
                            Dict.toList docs
                                |> List.map (\( k, v ) -> ( String.fromInt k, v ))
                                |> filterByValue

                        K.SKey docs ->
                            Dict.toList docs
                                |> filterByValue

                intTotal : Int
                intTotal =
                    List.length searched

                is : List ( String, M.Interlinear )
                is =
                    List.take params.length searched

                len : String
                len =
                    String.fromInt params.length

                imodel : Display.KeyedInterlinearListing.Model
                imodel =
                    { interlinears = is
                    , project = project
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
                , Html.article []
                    [ Html.header []
                        [ Html.h2 []
                            [ Html.text ("Sequence: " ++ seqd.title) ]
                        ]
                    , Html.p [] [ Html.text seqd.description ]
                    , case M.stringToIdentifier seqd.id of
                        Just id ->
                            Html.footer []
                                [ Html.a
                                    [ Attr.href "#"
                                    , Msg.ONoteFor
                                        { id = id
                                        , title = seqd.title
                                        , description = seqd.description
                                        }
                                        |> Msg.Request project
                                        |> Msg.UserClick
                                        |> Ms
                                        |> Event.onClick
                                    ]
                                    [ Html.text "Note" ]
                                ]

                        Nothing ->
                            Html.text ""
                    ]
                , Display.KeyedInterlinearListing.view imodel |> Html.map Ms
                ]

        Nothing ->
            Html.text "Missing project information"


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
                    { doc = Nothing
                    , tags = []
                    , properties = []
                    , modifications = []
                    , links = []
                    }
            in
            Ok <| List.foldl M.compositeBuilder initial content_

        Err e ->
            Err e


search : String -> List ( String, String ) -> Bool
search query strings =
    let
        strings_ : List ( String, String )
        strings_ =
            if String.any Unicode.isUpper query then
                strings

            else
                List.map (\( x, y ) -> ( x, String.toLower y )) strings
    in
    List.any (\( _, y ) -> String.contains query y) strings_


handleNewSequence : Model -> UUID.UUID -> ( Model, Cmd Msg )
handleNewSequence model project =
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

        int : Form.Sequence.Model
        int =
            Form.Sequence.initData step.uuid

        id : String
        id =
            "FORM::" ++ UUID.toString step.uuid

        vista : Vista
        vista =
            { identifier = id
            , path = "sequence/" ++ UUID.toString step.uuid
            , project = UUID.toString project
            , content = Content.SQE int
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
            { title = key ++ ": New Sequence"
            , fullTitle = title ++ ": New Sequence"
            , vista = id
            , params = Tab.defVParams
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


handleEditSequence : Model -> UUID.UUID -> M.Sequence -> ( Model, Cmd Msg )
handleEditSequence model project seq =
    let
        id : String
        id =
            "FORM::" ++ UUID.toString seq.id
    in
    case getByVista id model.tabs.ventanas of
        -- The edit tab is already open.
        Just tp ->
            ( model, sendMsg (Tab <| Tab.Goto tp) )

        Nothing ->
            let
                s : { model : Form.Sequence.Model, cmd : Cmd Form.Sequence.Msg }
                s =
                    let
                        ( smodel, scmd ) =
                            Form.Sequence.init seq
                    in
                    { model = smodel, cmd = scmd }

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
                        , seq.title
                        ]

                short : String
                short =
                    if String.length seq.title > 5 then
                        String.concat
                            [ key
                            , ": "
                            , "Edit: "
                            , String.left 7 seq.title
                            , "..."
                            ]

                    else
                        String.concat
                            [ key
                            , ": "
                            , "Edit: "
                            , seq.title
                            ]

                vista : Vista
                vista =
                    { identifier = id
                    , path = "sequence/" ++ id
                    , project = UUID.toString project
                    , content = Content.SQE s.model
                    }

                ventana : Ventana
                ventana =
                    { title = short
                    , fullTitle = full
                    , vista = id
                    , params = Tab.defVParams
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
                , Cmd.map (SQE seq.id) s.cmd
                ]
            )


handleNewInterlinear : Model -> UUID.UUID -> ( Model, Cmd Msg )
handleNewInterlinear model project =
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
            , path = "interlinear/" ++ UUID.toString step.uuid
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
            , params = Tab.defVParams
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


handleEditInterlinear : Model -> UUID.UUID -> M.Interlinear -> ( Model, Cmd Msg )
handleEditInterlinear model project int =
    let
        id : String
        id =
            "FORM::" ++ UUID.toString int.id
    in
    case getByVista id model.tabs.ventanas of
        -- The edit tab is already open.
        Just tp ->
            ( model, sendMsg (Tab <| Tab.Goto tp) )

        Nothing ->
            let
                i : { model : Form.Interlinear.Model, cmd : Cmd Form.Interlinear.Msg }
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
                    , path = "interlinear/" ++ id
                    , project = UUID.toString project
                    , content = Content.ITE i.model
                    }

                ventana : Ventana
                ventana =
                    { title = short
                    , fullTitle = full
                    , vista = id
                    , params = Tab.defVParams
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


handleOReversal : Model -> UUID.UUID -> String -> ( Model, Cmd Msg )
handleOReversal model project query =
    let
        envelope : E.Value
        envelope =
            envelopeEncoder
                { command = "request-reversal"
                , project = project
                , address = query
                , content = E.null
                }
    in
    ( model, send envelope )


handleOInterlinearListing : Model -> UUID.UUID -> ( Model, Cmd Msg )
handleOInterlinearListing model project =
    let
        envelope : E.Value
        envelope =
            envelopeEncoder
                { command = "request-interlinear-listing"
                , project = project
                , address = "interlinear"
                , content = E.null
                }

        project_ : String
        project_ =
            UUID.toString project
    in
    ( { model | loading = Set.insert project_ model.loading }
    , send envelope
    )


handleOSequenceListing : Model -> UUID.UUID -> ( Model, Cmd Msg )
handleOSequenceListing model project =
    let
        envelope : E.Value
        envelope =
            envelopeEncoder
                { command = "request-sequence-listing"
                , project = project
                , address = "sequence"
                , content = E.null
                }

        project_ : String
        project_ =
            UUID.toString project
    in
    ( { model | loading = Set.insert project_ model.loading }
    , send envelope
    )


handleOPersonListing : Model -> UUID.UUID -> ( Model, Cmd Msg )
handleOPersonListing model project =
    let
        envelope : E.Value
        envelope =
            envelopeEncoder
                { command = "request-person-listing"
                , project = project
                , address = "person"
                , content = E.null
                }
    in
    ( model, send envelope )


handleOComposite : Model -> UUID.UUID -> String -> ( Model, Cmd Msg )
handleOComposite model project id =
    let
        envelope : E.Value
        envelope =
            envelopeEncoder
                { command = "request-composite"
                , project = project
                , address = id
                , content = E.null
                }
    in
    ( model, send envelope )


handleOSequence : Model -> UUID.UUID -> String -> ( Model, Cmd Msg )
handleOSequence model project id =
    let
        envelope : E.Value
        envelope =
            envelopeEncoder
                { command = "request-sequence"
                , project = project
                , address = id
                , content = E.null
                }
    in
    ( model, send envelope )


handleODelete : Model -> UUID.UUID -> String -> String -> ( Model, Cmd Msg )
handleODelete model project rev id =
    let
        payload : E.Value
        payload =
            E.object
                [ ( "_id", E.string id )
                , ( "_rev", E.string rev )
                , ( "_deleted", E.bool True )
                ]

        path : String
        path =
            Tab.getFocusedVista model.tabs
                |> Maybe.map .path
                |> Maybe.withDefault ""

        envelope : E.Value
        envelope =
            envelopeEncoder
                { command = "delete-doc"
                , project = project
                , address = path
                , content = payload
                }
    in
    ( model, send envelope )


handleONoteFor : Model -> UUID.UUID -> M.GenericDesc -> ( Model, Cmd Msg )
handleONoteFor model project gd =
    let
        envelope : E.Value
        envelope =
            envelopeEncoder
                { command = "request-note-for"
                , project = project
                , address = M.identifierToString gd.id
                , content = M.genericDescEncoder gd
                }
    in
    ( model, send envelope )


handleONote : Model -> UUID.UUID -> String -> ( Model, Cmd Msg )
handleONote model project id =
    let
        envelope : E.Value
        envelope =
            envelopeEncoder
                { command = "request-note"
                , project = project
                , address = id
                , content = E.null
                }
    in
    ( model, send envelope )


handleISequenceComposite : Model -> Envelope -> M.Composite -> M.Sequence -> ( Model, Cmd Msg )
handleISequenceComposite model env doc seq =
    let
        full : String
        full =
            String.join " " [ "Sequence:", seq.title ]

        short : String
        short =
            if String.length seq.title > 7 then
                String.concat
                    [ "Sequence: "
                    , String.left 7 seq.title
                    , "..."
                    ]

            else
                full

        vista : Vista
        vista =
            { project = UUID.toString env.project
            , path = "sequence/" ++ UUID.toString seq.id
            , identifier = UUID.toString seq.id
            , content = Content.SQV doc
            }
    in
    handleVista vista short full model


handleIInterlinearComposite : Model -> Envelope -> M.Composite -> M.Interlinear -> ( Model, Cmd Msg )
handleIInterlinearComposite model env doc int =
    let
        full : String
        full =
            String.join " " [ "Gloss:", int.text ]

        short : String
        short =
            if String.length int.text > 7 then
                String.concat
                    [ "Gloss: "
                    , String.left 7 int.text
                    , "..."
                    ]

            else
                full

        vista : Vista
        vista =
            { project = UUID.toString env.project
            , path = "interlinear/" ++ UUID.toString int.id
            , identifier = UUID.toString int.id
            , content = Content.ITV doc
            }
    in
    handleVista vista short full model


handleIPersonComposite : Model -> Envelope -> M.Composite -> M.Person -> ( Model, Cmd Msg )
handleIPersonComposite model env doc person =
    let
        name : String
        name =
            person.names
                |> Dict.toList
                |> List.head
                |> Maybe.map Tuple.second
                |> Maybe.withDefault person.id

        full : String
        full =
            String.join " " [ "Person:", name ]

        short : String
        short =
            if String.length name > 7 then
                String.concat
                    [ "Person: "
                    , String.left 7 name
                    , "..."
                    ]

            else
                full

        vista : Vista
        vista =
            { project = UUID.toString env.project
            , path = "person/" ++ person.id
            , identifier = person.id
            , content = Content.PLV doc
            }
    in
    handleVista vista short full model


handleIComposite : Model -> E.Value -> ( Model, Cmd Msg )
handleIComposite model envelopeJson =
    case D.decodeValue envelopeDecoder envelopeJson of
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
                Err e ->
                    ( { model | error = D.errorToString e }
                    , Cmd.none
                    )

                Ok doc_ ->
                    case doc_.doc of
                        Just (M.MyInterlinear i) ->
                            handleIInterlinearComposite model env doc_ i

                        Just (M.MySequence s) ->
                            handleISequenceComposite model env doc_ s

                        Just (M.MyPerson p) ->
                            handleIPersonComposite model env doc_ p

                        _ ->
                            ( model, Cmd.none )


handleIReversal : Model -> E.Value -> ( Model, Cmd Msg )
handleIReversal model envelopeJson =
    case openEnvelope model envelopeJson M.listDecoder of
        Err m ->
            ( m, Cmd.none )

        Ok ( env, vals ) ->
            handleIReversal_ model env vals


handleIReversal_ : Model -> Envelope -> List M.Value -> ( Model, Cmd Msg )
handleIReversal_ model env vals =
    case M.reversalToIdentifier env.address of
        Nothing ->
            ( model, Cmd.none )

        Just (M.MyDocId _) ->
            ( model, Cmd.none )

        Just (M.MyUtilityId _) ->
            ( model, Cmd.none )

        Just (M.MyNoteId _) ->
            ( model, Cmd.none )

        Just (M.MyTagId tagid) ->
            handleReversalForDoctype model env vals tagid.docid tagid.kind

        Just (M.MyPropertyId propid) ->
            let
                title : String
                title =
                    propid.kind ++ ":" ++ propid.value
            in
            handleReversalForDoctype model env vals propid.docid title

        Just (M.MyModificationId modid) ->
            handleReversalForDoctype model env vals modid.docid modid.kind

        Just (M.MyLinkId linkid) ->
            handleReversalForDoctype model env vals linkid.fromid linkid.kind


handleReversalForDoctype : Model -> Envelope -> List M.Value -> M.DocId -> String -> ( Model, Cmd Msg )
handleReversalForDoctype model env vals docid title =
    let
        ( short, full, content ) =
            case docid of
                M.PersonId _ ->
                    let
                        filter : M.Value -> List M.Person -> List M.Person
                        filter val people =
                            case val of
                                M.MyPerson person ->
                                    person :: people

                                _ ->
                                    people

                        c : Content
                        c =
                            PLS <| List.foldl filter [] vals

                        f : String
                        f =
                            String.concat
                                [ "People: "
                                , title
                                ]

                        s : String
                        s =
                            if String.length title > 7 then
                                String.concat
                                    [ "People: "
                                    , String.left 7 title
                                    , "..."
                                    ]

                            else
                                f
                    in
                    ( s, f, c )

                M.InterlinearId _ ->
                    let
                        filter : M.Value -> List M.Interlinear -> List M.Interlinear
                        filter val ints =
                            case val of
                                M.MyInterlinear int ->
                                    int :: ints

                                _ ->
                                    ints

                        c : Content
                        c =
                            ITS <| List.foldl filter [] vals

                        f : String
                        f =
                            String.concat
                                [ "Glosses: "
                                , title
                                ]

                        s : String
                        s =
                            if String.length title > 7 then
                                String.concat
                                    [ "Glosses: "
                                    , String.left 7 title
                                    , "..."
                                    ]

                            else
                                f
                    in
                    ( s, f, c )

                M.SequenceId _ ->
                    let
                        filter : M.Value -> List M.Sequence -> List M.Sequence
                        filter val seqs =
                            case val of
                                M.MySequence seq ->
                                    seq :: seqs

                                _ ->
                                    seqs

                        c : Content
                        c =
                            SQS <| List.foldl filter [] vals

                        f : String
                        f =
                            String.concat
                                [ "Sequences: "
                                , title
                                ]

                        s : String
                        s =
                            if String.length title > 7 then
                                String.concat
                                    [ "Sequences: "
                                    , String.left 7 title
                                    , "..."
                                    ]

                            else
                                f
                    in
                    ( s, f, c )

                M.PageId _ ->
                    let
                        filter : M.Value -> List M.Page -> List M.Page
                        filter val pages =
                            case val of
                                M.MyPage page ->
                                    page :: pages

                                _ ->
                                    pages

                        c : Content
                        c =
                            PGS <| List.foldl filter [] vals

                        f : String
                        f =
                            String.concat
                                [ "Pages: "
                                , title
                                ]

                        s : String
                        s =
                            if String.length title > 7 then
                                String.concat
                                    [ "Pages: "
                                    , String.left 7 title
                                    , "..."
                                    ]

                            else
                                f
                    in
                    ( s, f, c )

        vista : Vista
        vista =
            { project = UUID.toString env.project
            , path = env.address
            , identifier =
                String.concat
                    [ full
                    , "::"
                    , UUID.toString env.project
                    ]
            , content = content
            }
    in
    handleVista vista short full model


handleIPersonListing : Model -> E.Value -> ( Model, Cmd Msg )
handleIPersonListing model envelopeJson =
    case openEnvelope model envelopeJson M.listDecoder of
        Err m ->
            ( m, Cmd.none )

        Ok ( env, vals ) ->
            handleIPersonListing_ model env vals


handleIPersonListing_ : Model -> Envelope -> List M.Value -> ( Model, Cmd Msg )
handleIPersonListing_ model env vals =
    let
        filterPerson : M.Value -> List M.Person -> List M.Person
        filterPerson val people =
            case val of
                M.MyPerson person ->
                    person :: people

                _ ->
                    people

        content : Content
        content =
            PLS <| List.foldl filterPerson [] vals

        vista : Vista
        vista =
            { project = UUID.toString env.project
            , path = "person"
            , identifier = "PEOPLE::" ++ UUID.toString env.project
            , content = content
            }
    in
    handleVista vista "People" "People" model


handleISequence : Model -> E.Value -> ( Model, Cmd Msg )
handleISequence model envelopeJson =
    case openEnvelope model envelopeJson (D.list K.valueDecoder) of
        Err m ->
            ( m, Cmd.none )

        Ok ( env, kvals ) ->
            handleISequence_ model env kvals


handleISequence_ : Model -> Envelope -> List K.Value -> ( Model, Cmd Msg )
handleISequence_ model env kvals =
    case K.valuesToSeqData kvals of
        Nothing ->
            ( { model | error = "Keyed values did not resolve to valid SeqData" }
            , Cmd.none
            )

        Just seqData ->
            let
                content : Content
                content =
                    Content.SDV seqData

                vista : Vista
                vista =
                    { project = UUID.toString env.project
                    , path = "seqdata"
                    , identifier = "SEQDATA::" ++ UUID.toString env.project
                    , content = content
                    }
            in
            handleVista vista "Seq. Data" "Seq. Data" model


handleISequenceListing : Model -> E.Value -> ( Model, Cmd Msg )
handleISequenceListing model envelopeJson =
    case openEnvelope model envelopeJson M.listDecoder of
        Err m ->
            ( m, Cmd.none )

        Ok ( env, vals ) ->
            handleISequenceListing_ model env vals


handleISequenceListing_ : Model -> Envelope -> List M.Value -> ( Model, Cmd Msg )
handleISequenceListing_ model env vals =
    let
        filterSeq : M.Value -> List M.Sequence -> List M.Sequence
        filterSeq val seqs =
            case val of
                M.MySequence seq ->
                    seq :: seqs

                _ ->
                    seqs

        content : Content
        content =
            SQS <| List.foldl filterSeq [] vals

        vista : Vista
        vista =
            { project = UUID.toString env.project
            , path = "sequence"
            , identifier = "SEQUENCES::" ++ UUID.toString env.project
            , content = content
            }
    in
    handleVista vista "Sequences" "Sequences" model


handleIInterlinearListing : Model -> E.Value -> ( Model, Cmd Msg )
handleIInterlinearListing model envelopeJson =
    case openEnvelope model envelopeJson M.listDecoder of
        Err m ->
            ( m, Cmd.none )

        Ok ( env, vals ) ->
            handleIInterlinearListing_ model env vals


handleIInterlinearListing_ : Model -> Envelope -> List M.Value -> ( Model, Cmd Msg )
handleIInterlinearListing_ model env vals =
    let
        filterInter : M.Value -> List M.Interlinear -> List M.Interlinear
        filterInter val ints =
            case val of
                M.MyInterlinear int ->
                    int :: ints

                _ ->
                    ints

        content : Content
        content =
            ITS <| List.foldl filterInter [] vals

        vista : Vista
        vista =
            { project =
                UUID.toString env.project
            , path =
                "interlinear"
            , identifier =
                "GLOSSES::"
                    ++ UUID.toString env.project
            , content =
                content
            }
    in
    handleVista vista "Glosses" "Glosses" model


handleIViewArea : Model -> E.Value -> ( Model, Cmd Msg )
handleIViewArea model jsonValue =
    case D.decodeValue dimensionsDecoder jsonValue of
        Err e ->
            ( { model | error = D.errorToString e }
            , Cmd.none
            )

        Ok dimensions ->
            ( { model | viewArea = dimensions }
            , Cmd.none
            )


handleIGlobalConfig : Model -> E.Value -> ( Model, Cmd Msg )
handleIGlobalConfig model gcJson =
    case D.decodeValue Config.globalConfigDecoder gcJson of
        Err err ->
            ( { model | error = D.errorToString err }
            , Cmd.none
            )

        Ok gc ->
            let
                invalidPerson : Bool
                invalidPerson =
                    String.isEmpty gc.name || String.isEmpty gc.email

                newmodel : Model
                newmodel =
                    { model
                        | gconfig = gc
                        , me =
                            if invalidPerson then
                                Nothing

                            else
                                Just
                                    { id = gc.email
                                    , rev = Nothing
                                    , version = 1
                                    , names =
                                        Dict.singleton 0 gc.name
                                    }
                    }

                -- When the data is incomplete, open the form
                -- so the user can add their name and email.
                command : Cmd Msg
                command =
                    if invalidPerson then
                        sendMsg (ShowGlobalSettings gcJson)

                    else
                        Cmd.none
            in
            ( newmodel, command )


handleINote : Model -> E.Value -> ( Model, Cmd Msg )
handleINote model envelopeJson =
    case openEnvelope model envelopeJson M.decoder of
        Err m ->
            ( m, Cmd.none )

        Ok ( env, M.MyNote note ) ->
            handleINote_ model env note

        _ ->
            ( model, Cmd.none )


handleINote_ : Model -> Envelope -> M.Note -> ( Model, Cmd Msg )
handleINote_ model env note =
    let
        vid : String
        vid =
            "NOTE::" ++ M.identifierToString (M.MyNoteId note.id)
    in
    case Dict.get vid model.tabs.vistas of
        Nothing ->
            ( model, Cmd.none )

        Just vista ->
            case vista.content of
                NTV nmodel ->
                    let
                        nmodel_ : Display.Note.Model
                        nmodel_ =
                            { nmodel | note = note, original = note.note }

                        content_ : Content
                        content_ =
                            NTV nmodel_

                        vista_ : Tab.Vista
                        vista_ =
                            { vista | content = content_ }

                        tabs : Tab.Model
                        tabs =
                            model.tabs

                        vistas : Tab.Vistas
                        vistas =
                            Dict.insert vid vista_ tabs.vistas
                    in
                    ( { model | tabs = { tabs | vistas = vistas } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


handleINoteFor : Model -> E.Value -> ( Model, Cmd Msg )
handleINoteFor model envelopeJson =
    case D.decodeValue envelopeDecoder envelopeJson of
        Err e ->
            ( { model | error = D.errorToString e }
            , Cmd.none
            )

        Ok env ->
            let
                dnote : M.Value -> D.Decoder M.Note
                dnote n =
                    case n of
                        M.MyNote note ->
                            D.succeed note

                        _ ->
                            D.fail "Expected a Note"

                ndecoder : D.Decoder { note : M.Note, desc : M.GenericDesc }
                ndecoder =
                    D.map2
                        (\n d -> { note = n, desc = d })
                        (D.field "note" M.decoder |> D.andThen dnote)
                        (D.field "desc" M.genericDescDecoder)
            in
            case D.decodeValue ndecoder env.content of
                Err e ->
                    ( { model | error = D.errorToString e }
                    , Cmd.none
                    )

                Ok c ->
                    handleReceivedNote env.project model c.desc c.note


handleIReload : Model -> E.Value -> ( Model, Cmd Msg )
handleIReload model envelopeJson =
    case D.decodeValue envelopeDecoder envelopeJson of
        Err e ->
            ( { model | error = D.errorToString e }
            , Cmd.none
            )

        Ok env ->
            let
                req : RequestType -> Cmd Msg
                req rt =
                    rt |> Msg.Request env.project |> Ms |> sendMsg
            in
            case String.split "/" env.address of
                [ "interlinear" ] ->
                    ( model
                    , req OInterlinearListing
                    )

                [ "sequence" ] ->
                    ( model
                    , req OSequenceListing
                    )

                [ "person" ] ->
                    ( model
                    , req OPersonListing
                    )

                "interlinear" :: _ :: [] ->
                    ( model
                    , req <| OComposite env.address
                    )

                "sequence" :: _ :: [] ->
                    ( model
                    , req <| OComposite env.address
                    )

                "interlinear" :: _ :: "note" :: [] ->
                    ( model
                    , req <| ONote env.address
                    )

                "sequence" :: _ :: "note" :: [] ->
                    ( model
                    , req <| ONote env.address
                    )

                "tag" :: _ :: _ :: [] ->
                    ( model
                    , req <| OReversal (Just env.address)
                    )

                "property" :: _ :: _ :: _ :: [] ->
                    ( model
                    , req <| OReversal (Just env.address)
                    )

                _ ->
                    ( model, Cmd.none )


handleStatus : Model -> E.Value -> ( Model, Cmd Msg )
handleStatus model envelopeJson =
    case openEnvelope model envelopeJson (D.list D.string) of
        Err m ->
            ( m, Cmd.none )

        Ok ( env, stat ) ->
            handleStatus_ model env stat


handleStatus_ : Model -> Envelope -> List String -> ( Model, Cmd Msg )
handleStatus_ model env stat =
    let
        curr : AL.Dict ProjectId (Dict String Int)
        curr =
            model.status

        newmsgs : Dict String Int
        newmsgs =
            stat
                |> List.map (\s -> ( s, 2 ))
                |> Dict.fromList

        natMinus1 : Int -> Int
        natMinus1 i =
            if i <= 0 then
                i

            else
                i - 1

        demote : List String -> Dict String Int -> Dict String Int
        demote s mgs =
            Dict.map
                (\k v ->
                    if List.member k s then
                        2

                    else
                        natMinus1 v
                )
                mgs

        new : AL.Dict ProjectId (Dict String Int)
        new =
            case AL.get env.project curr of
                Nothing ->
                    AL.insert env.project
                        newmsgs
                        curr

                Just smsgs ->
                    demote stat smsgs
                        |> Dict.union newmsgs
                        |> (\x -> AL.insert env.project x curr)
    in
    ( { model | status = new }, Cmd.none )


handleNoteChange : Model -> String -> String -> ( Model, Cmd Msg )
handleNoteChange model vistaid str =
    case Dict.get vistaid model.tabs.vistas of
        Nothing ->
            ( model, Cmd.none )

        Just vista ->
            case vista.content of
                NTV nmodel ->
                    let
                        note : M.Note
                        note =
                            nmodel.note

                        nmodel_ : Display.Note.Model
                        nmodel_ =
                            { nmodel | note = { note | note = str } }

                        content_ : Content
                        content_ =
                            NTV nmodel_

                        vista_ : Tab.Vista
                        vista_ =
                            { vista | content = content_ }

                        tabs : Tab.Model
                        tabs =
                            model.tabs

                        vistas : Dict String Vista
                        vistas =
                            Dict.insert vistaid
                                vista_
                                model.tabs.vistas
                    in
                    ( { model | tabs = { tabs | vistas = vistas } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


openEnvelope : Model -> E.Value -> D.Decoder a -> Result Model ( Envelope, a )
openEnvelope model envelopeJson decoder =
    case D.decodeValue envelopeDecoder envelopeJson of
        Err e ->
            Err { model | error = D.errorToString e }

        Ok env ->
            case D.decodeValue decoder env.content of
                Err e ->
                    Err { model | error = D.errorToString e }

                Ok content ->
                    Ok ( env, content )



{- PORTS -}


port send : E.Value -> Cmd msg



-- {-| The window title changes depending on the focused tab. This sends
-- the signal to the backend to do so.
-- -}
--port setWindowTitle : String -> Cmd msg


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


port receivedInterlinearListing : (E.Value -> msg) -> Sub msg


port receivedSequenceListing : (E.Value -> msg) -> Sub msg


port receivedSequence : (E.Value -> msg) -> Sub msg


port receivedPersonListing : (E.Value -> msg) -> Sub msg


port receivedReloadRequest : (E.Value -> msg) -> Sub msg


port receivedInterlinearReversals : (E.Value -> msg) -> Sub msg


port receivedComposite : (E.Value -> msg) -> Sub msg


port receivedNoteFor : (E.Value -> msg) -> Sub msg


port receivedNote : (E.Value -> msg) -> Sub msg


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


port moveLeft : (E.Value -> msg) -> Sub msg


port moveRight : (E.Value -> msg) -> Sub msg


port moveUp : (E.Value -> msg) -> Sub msg


port moveDown : (E.Value -> msg) -> Sub msg


port closeTab : (E.Value -> msg) -> Sub msg


port cloneTab : (E.Value -> msg) -> Sub msg


port toggleSidebar : (() -> msg) -> Sub msg


port status : (E.Value -> msg) -> Sub msg
