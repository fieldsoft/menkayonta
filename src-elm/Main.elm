port module Main exposing (main)

import Browser
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
        , visMember
        , tpToS
        , tabpath
        , treeifyTabs
        , getByVista
        , update
        )
import Task
import Time
import UUID
import Url


type alias Model =
    { gconfig : Maybe GlobalConfig
    , me : Maybe M.Person
    , tabs : Tab.Model
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
      , tabs = Tab.initData gv
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

        Tab subMsg ->
            let
                ( subModel, subCmd ) =
                    Tab.update subMsg model.tabs
            in
            ( { model | tabs = subModel }
            , Cmd.map Tab subCmd
            )

        ITE id subMsg ->
            let
                id_ =
                    "FORM::" ++ UUID.toString id

                maybeVista =
                    Dict.get id_ model.tabs.vistas

                maybeTab =
                    getByVista id_ model.tabs.ventanas
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
                            Dict.insert id_ vista model.tabs.vistas

                        tabs =
                            model.tabs

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
                    Dict.get "import-options" model.tabs.vistas
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
                    Dict.insert "import-options" vista model.tabs.vistas

                tabs =
                    model.tabs

                nmodel =
                    { model | tabs = { tabs | vistas = vistas } }
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
                    Dict.get "global-settings" model.tabs.vistas
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
                    Dict.insert "global-settings" vista model.tabs.vistas

                tabs =
                    model.tabs
                        
                nmodel =
                    { model | tabs = { tabs | vistas = vistas } }
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
                    case Dict.get vistaId model.tabs.vistas of
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
                    Dict.insert vistaId vista model.tabs.vistas

                tabs =
                    model.tabs

                nmodel =
                    { model | tabs = { tabs | vistas = vistas } }
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
                    Dict.insert "import-options" vista model.tabs.vistas

                tabs =
                    model.tabs
                        
                newmodel =
                    { model | tabs = { tabs | vistas = vistas } }
            in
            case getByVista "import-options" model.tabs.ventanas of
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
                    Dict.insert "global-settings" vista model.tabs.vistas

                tabs =
                    model.tabs

                newmodel =
                    { model | tabs = { tabs | vistas = vistas } }
            in
            case getByVista "global-settings" model.tabs.ventanas of
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
                    Dict.insert vistaId vista model.tabs.vistas

                tabs =
                    model.tabs

                newmodel =
                    { model | tabs = { tabs | vistas = vistas } }
            in
            case getByVista vistaId model.tabs.ventanas of
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
                    Dict.insert vistaId vista model.tabs.vistas

                tabs =
                    model.tabs

                newmodel =
                    { model | tabs = { tabs | vistas = vistas } }
            in
            case getByVista vistaId model.tabs.ventanas of
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

                tabs =
                    model.tabs
            in
            ( { model
                | seeds = seeds
                , tabs =
                    { tabs
                        | vistas = Dict.insert id vista model.tabs.vistas
                    }
              }
            , sendMsg (Tab <| Tab.New ventana)
            )

        EditInterlinear project int ->
            let
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
                            Dict.insert id vista model.tabs.vistas

                        tabs =
                            model.tabs
                    in
                    ( { model | tabs = { tabs | vistas = vistas } }
                    , Cmd.batch
                        [ sendMsg (Tab <| Tab.New ventana)
                        , Cmd.map (ITE int.id) subCmd
                        ]
                    )

        ChangeLengthParam tp str ->
            case String.toInt str of
                Just i ->
                    case Dict.get tp model.tabs.ventanas of
                        Just ventana ->
                            let
                                params =
                                    ventana.params

                                np =
                                    { params | length = i }

                                nv =
                                    { ventana | params = np }

                                nvs =
                                    Dict.insert tp nv model.tabs.ventanas

                                tabs =
                                    model.tabs
                            in
                            ( { model | tabs = { tabs | ventanas = nvs } }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ChangeSearchParam tp str ->
            case Dict.get tp model.tabs.ventanas of
                Just ventana ->
                    let
                        params =
                            ventana.params

                        np =
                            { params | searchString = str }

                        nv =
                            { ventana | params = np }

                        nvs =
                            Dict.insert tp nv model.tabs.ventanas

                        tabs =
                            model.tabs
                    in
                    ( { model | tabs = { tabs | ventanas = nvs } }
                    , Cmd.none
                    )

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
                    Dict.insert vista.identifier vista model.tabs.vistas

                loading =
                    if vista.kind /= "interlinear" then
                        Set.remove vista.project model.loading

                    else
                        model.loading

                tabs =
                    model.tabs
                        
                newmodel =
                    { model
                        | tabs = { tabs | vistas = vistas }
                        , loading = loading
                    }
            in
            case getByVista vista.identifier model.tabs.ventanas of
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
            treeifyTabs <| Dict.keys model.tabs.ventanas
    in
    Html.main_ []
        [ Html.aside [ Attr.class "side" ]
            [ Html.nav []
                [ Html.h4 [] [ Html.text "Projects" ]
                , viewProjects model
                , if Dict.isEmpty model.tabs.visVentanas then
                    Html.text ""

                  else
                    Html.h4 [] [ Html.text "Open Tabs" ]
                , viewTabList model.tabs.ventanas
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
            Dict.get tp model.tabs.ventanas
                |> Maybe.withDefault
                    { title = "Error"
                    , fullTitle = "Error"
                    , vista = "Vista not found"
                    , params = defVParams
                    }

        focused =
            Just tp == model.tabs.focused

        visible =
            visMember tp model.tabs.visVentanas
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
                    Dict.get tp model.tabs.ventanas
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


{-| I'm doing a one off here, instead of adding it to the Menkayonta
module because I want to eventually be able to handle all Menkayonta
Values in the UI.
-}
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
