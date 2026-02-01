port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Config exposing (GlobalConfig, GlobalSettings, ProjectInfo)
import Content
    exposing
        ( Content(..)
        , Translation
        , translationsDecoder
        )
import Dict exposing (Dict)
import Email exposing (Email)
import Form.Interlinear
import Form
    exposing
        ( CForm(..)
        , FieldId(..)
        , GlobalField(..)
        , GlobalFormData
        , ProjectField(..)
        , ProjectFormData
        , ImportFormData
        , ImportField(..)
        , globalFormData
        , projectFormData
        , importFormData
        )
import Form.Shared exposing (blankString, blankSelect)
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
import Msg exposing (..)
import Random
import Result
import Set exposing (Set)
import Tab
    exposing
        ( Direction(..)
        , TabPath
        , Ventana
        , VentanaParams
        , Ventanas
        , VisVentanas
        , Vista
        , Vistas
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
    , focused : Maybe TabPath
    , visVentanas : VisVentanas
    , vistas : Vistas
    , error : String
    , loading : Set String
    , people : Dict String (Dict String M.Person)
    , seeds : UUID.Seeds
    , time : Time.Posix
    }


type alias ImportOptions =
    { filepath : String
    , kind : String
    , project : String
    }


type alias Error =
    { message : String }


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
        (D.field "identifier" D.string)
        (D.field "address" D.string)
        (D.field "content" D.value)


envelopeEncoder : Envelope -> E.Value
envelopeEncoder env =
    E.object
        [ ( "command", E.string env.command )
        , ( "identifier", E.string env.project )
        , ( "address", E.string env.address )
        , ( "content", env.content )
        ]


type alias DocReq =
    { command : String
    , identifier : String
    , docid : String
    }


type alias DocRec =
    { command : String
    , identifier : String
    , doc : Content
    }


docReqEncode : DocReq -> E.Value
docReqEncode dr =
    E.object
        [ ( "command", E.string dr.command )
        , ( "identifier", E.string dr.identifier )
        , ( "docid", E.string dr.docid )
        ]


{-| These are not specific to any project and are kept around, even
when not in use.
-}
globalVistas : Dict String Vista
globalVistas =
    [ ( "new-project"
      , { project = "global"
        , kind = "new-project"
        , identifier = "new-project"
        , content = ProjectInfoContent (ProjectCForm projectFormData)
        }
      )
    , ( "import-options"
      , { project = "global"
        , kind = "import-options"
        , identifier = "import-options"
        , content = ImportOptionsContent (ImportCForm importFormData)
        }
      )
    , ( "global-settings"
      , { project = "global"
        , kind = "global-settings"
        , identifier = "global-settings"
        , content = GlobalSettingsContent (GlobalCForm globalFormData)
        }
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


defVParams : VentanaParams
defVParams =
    { length = 0
    , searchString = ""
    , edit = False
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
    in
    ( { gconfig = Nothing
      , me = Nothing
      , counter = 0
      , ventanas = Dict.empty
      , focused = Nothing
      , visVentanas = Dict.empty
      , vistas = globalVistas
      , error = ""
      , loading = Set.empty
      , people = Dict.empty
      , seeds = seeds
      , time = Time.millisToPosix 0
      }
    , Cmd.batch
        [ Task.perform SetTime Time.now
        , requestGlobalConfig ()
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

        SetTime time ->
            ( { model | time = time }, Cmd.none )

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

        GotoTab tp ->
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
                , sendMsg (FocusTab tp)
                ]
            )

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

        CloseTab ->
            let
                tp =
                    Maybe.withDefault (tabpath -1 -1 -1) <| model.focused
            in
            ( closeTab True tp model, Cmd.none )

        CloneTab ->
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
                    update (NewTab v) model

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
                    ( { model | error = D.errorToString err }
                    , Cmd.none
                    )

                Ok gc_ ->
                    let
                        person =
                            gc_.email
                                |> Maybe.andThen
                                    (\email ->
                                        gc_.name
                                            |> Maybe.map
                                                (\name ->
                                                    { id = email
                                                    , rev = Nothing
                                                    , version = 1
                                                    , names =
                                                        Dict.singleton 0 name
                                                    }
                                                )
                                    )

                        newmodel =
                            { model
                                | gconfig = Just gc_
                                , me = person
                            }

                        openMenu =
                            sendMsg (GlobalSettingsMenu gc)

                        command =
                            case ( gc_.name, gc_.email ) of
                                ( Nothing, _ ) ->
                                    openMenu

                                ( _, Nothing ) ->
                                    openMenu

                                _ ->
                                    Cmd.none
                    in
                    ( newmodel, command )

        RequestProjectIndex id ->
            ( { model | loading = Set.insert id model.loading }
            , Cmd.batch
                [ requestProjectIndex id
                , requestPersonIndex id
                ]
            )

        RequestInterlinearIndex id ->
            ( { model | loading = Set.insert id model.loading }
            , Cmd.batch
                [ requestInterlinearIndex id
                , requestPersonIndex id
                ]
            )

        RequestDocId project id ->
            let
                payload =
                    docReqEncode
                        { command = "request-docid"
                        , identifier = project
                        , docid = id
                        }
            in
            ( model, requestDocId payload )

        RequestAllDocId project id ->
            let
                payload =
                    envelopeEncoder
                        { command = "request-all-docid"
                        , project = project
                        , address = id
                        , content = E.null
                        }
            in
            ( model, requestAllDocId payload )

        ReceivedInterlinearIndex ii ->
            handleReceivedVista ii "Glosses" "Glosses" model

        ReceivedPersonIndex envelope ->
            case D.decodeValue envelopeDecoder envelope of
                Ok envelope_ ->
                    case D.decodeValue M.listDecoder envelope_.content of
                        Ok vals ->
                            let
                                project =
                                    envelope_.project

                                key p =
                                    case Dict.get 0 p.names of
                                        Nothing ->
                                            String.join " "
                                                [ "Anonymous"
                                                , p.id
                                                ]

                                        Just name ->
                                            String.join " "
                                                [ name ++ ","
                                                , p.id
                                                ]

                                pdict =
                                    M.people vals
                                        |> List.map
                                            (\p ->
                                                ( key p, p )
                                            )
                                        |> Dict.fromList
                            in
                            ( { model
                                | people =
                                    Dict.insert project pdict model.people
                              }
                            , Cmd.none
                            )

                        Err e ->
                            ( { model | error = D.errorToString e }
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

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
                                                DocContent
                                                    { view = doc_
                                                    , edit = Nothing
                                                    }
                                            }
                                    in
                                    handleVista vista short full model

                                _ ->
                                    ( model, Cmd.none )

                        Err e ->
                            ( { model | error = D.errorToString e }
                            , Cmd.none
                            )

        ReceivedDoc val ->
            -- This will have uses, eventually. There will be cases of
            -- having received a specific document, but for working
            -- with interlinear glosses and people, the (currently
            -- named) ReceivedAllDoc is used.
            ( model, Cmd.none )

        -- Open or focus the Import Options form with a filename.
        ImportOptionsFileMenu filepath ->
            let
                gc =
                    case model.gconfig of
                        Nothing ->
                            []

                        Just gconf ->
                            gconf.projects
                                |> List.map
                                    (\x ->
                                        ( x.title
                                        , x.identifier
                                        )
                                    )

                formData =
                    { importFormData
                        | filepath = filepath
                        , kind =
                            { blankSelect
                                | options =
                                    [ ( "Dative Form Json"
                                      , "Dative Form Json"
                                      )
                                    ]
                            }
                        , project =
                            { blankSelect | options = gc }
                    }

                content =
                    ImportOptionsContent (ImportCForm formData)

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
                    , sendMsg (NewTab ventana)
                    )

                Just tp ->
                    ( newmodel
                    , sendMsg (GotoTab tp)
                    )

        -- Open or focus the Global Settings form with updated global
        -- configuration.
        GlobalSettingsMenu value ->
            case D.decodeValue globalConfigDecoder value of
                Err _ ->
                    ( model, Cmd.none )

                Ok gf ->
                    let
                        gs =
                            GlobalSettings
                                (Maybe.withDefault "" gf.name)
                                (Maybe.withDefault "" gf.email)

                        formData =
                            { globalFormData
                                | email =
                                    { blankString | value = gs.email }
                                , name =
                                    { blankString | value = gs.name }
                            }

                        content =
                            GlobalSettingsContent (GlobalCForm formData)

                        vista =
                            { project = "global"
                            , kind = "global-settings"
                            , identifier = "global-settings"
                            , content = content
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
                            , sendMsg (NewTab newVentana)
                            )

                        Just tp ->
                            ( newmodel
                            , sendMsg (GotoTab tp)
                            )

        FormInit _ (ImportCForm imp) ->
            ( model, Cmd.none )

        FormInit _ (GlobalCForm imp) ->
            ( model, Cmd.none )

        FormInit "" (ProjectCForm prj) ->
            let
                ( uuid, seeds ) =
                    UUID.step model.seeds

                prj_ =
                    { prj | identifier = UUID.toString uuid }

                vista =
                    { project = "global"
                    , kind = "new-project"
                    , identifier = "new-project"
                    , content =
                        ProjectInfoContent (ProjectCForm prj_)
                    }

                vistas =
                    Dict.insert "new-project" vista model.vistas

                newmodel =
                    { model | vistas = vistas }
            in
            case getByVista "new-project" model.ventanas of
                Nothing ->
                    let
                        ventana =
                            { title = "New Project"
                            , fullTitle = "New Project"
                            , vista = "new-project"
                            , params = defVParams
                            }
                    in
                    ( newmodel, sendMsg (NewTab ventana) )

                Just tp ->
                    ( newmodel, sendMsg (GotoTab tp) )

        FormInit project (ProjectCForm prj) ->
            let
                piid =
                    "edit-project::" ++ project

                tabtitle =
                    prj.title.value ++ " Settings"

                vista =
                    { project = project
                    , kind = "edit-project"
                    , identifier = piid
                    , content = ProjectInfoContent (ProjectCForm prj)
                    }

                vistas =
                    Dict.insert piid vista model.vistas

                newmodel =
                    { model | vistas = vistas }
            in
            case getByVista piid model.ventanas of
                Nothing ->
                    let
                        ventana =
                            { title = tabtitle
                            , fullTitle = tabtitle
                            , vista = piid
                            , params = defVParams
                            }
                    in
                    ( newmodel, sendMsg (NewTab ventana) )

                Just tp ->
                    ( newmodel, sendMsg (GotoTab tp) )

        FormInit project (InterlinearCForm int) ->
            let
                ( uuid, seeds ) =
                    UUID.step model.seeds

                nint =
                    { int| id = Just uuid }

                nid =
                    uuid
                        |> M.InterlinearId
                        |> M.MyDocId
                        |> M.identifierToString

                vista : Vista
                vista =
                    { identifier = nid
                    , kind = "interlinear"
                    , project = project
                    , content = NewDocContent (InterlinearCForm nint)
                    }

                ventana : Ventana
                ventana =
                    { title = "New Gloss"
                    , fullTitle = "New Gloss"
                    , vista = nid
                    , params = { length = 0
                               , searchString = ""
                               , edit = True
                               }
                    }
            in
            ( { model
                | seeds = seeds
                , vistas = Dict.insert nid vista model.vistas
              }
            , sendMsg (NewTab ventana)
            )

        FormSubmit tp ->
            let
                formid =
                    Dict.get tp model.ventanas |> Maybe.map .vista
            in
            case formid of
                Just ident ->
                    let
                        contentVista =
                            getContentVistaFromVistas
                                ident
                                model.vistas
                    in
                    case contentVista of
                        Just ( DocContent dc, vista ) ->
                            let
                                ( nmodel, ncmd ) =
                                    handleCFormSubmit
                                        dc.edit
                                        vista
                                        model
                            in
                            ( nmodel, ncmd )

                        Just ( NewDocContent fd, vista ) ->
                            let
                                ( nmodel, ncmd ) =
                                    handleCFormSubmit
                                        (Just fd)
                                        vista
                                        model
                            in
                            ( nmodel
                            , Cmd.batch
                                [ sendMsg CloseTab
                                , ncmd
                                ]
                            )

                        Just ( ImportOptionsContent imf, vista ) ->
                            let
                                ( nmodel, ncmd ) =
                                    handleCFormSubmit
                                        (Just imf)
                                        vista
                                        model
                            in
                            ( nmodel
                            , Cmd.batch
                                [ sendMsg CloseTab
                                , ncmd
                                ]
                            )

                        Just ( GlobalSettingsContent glf, vista ) ->
                            let
                                ( nmodel, ncmd ) =
                                    handleCFormSubmit
                                        (Just glf)
                                        vista
                                        model
                            in
                            ( nmodel
                            , Cmd.batch
                                [ sendMsg CloseTab
                                , ncmd
                                ]
                            )

                        Just ( ProjectInfoContent prj, vista ) ->
                            let
                                ( nmodel, ncmd ) =
                                    handleCFormSubmit
                                        (Just prj)
                                        vista
                                        model
                            in
                            ( nmodel
                            , Cmd.batch
                                [ sendMsg CloseTab
                                , ncmd
                                ]
                            )

                        Just ( TranslationContent _, _ ) ->
                            ( model, Cmd.none )

                        Just ( TranslationsContent _, _ ) ->
                            ( model, Cmd.none )

                        Just ( InterlinearsContent _, _ ) ->
                            ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

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

        ChangeEditParam tp ->
            case Dict.get tp model.ventanas of
                Just ventana ->
                    let
                        params =
                            ventana.params

                        edit =
                            not params.edit

                        nvistas =
                            if edit then
                                maybeInitForm ventana.vista model.vistas

                            else
                                model.vistas

                        nv =
                            { ventana
                                | params = { params | edit = edit }
                            }

                        nvs =
                            Dict.insert tp nv model.ventanas
                    in
                    ( { model
                        | ventanas = nvs
                        , vistas = nvistas
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        FormChange tp fid str ->
            case getContentVistaVentana tp model of
                Nothing ->
                    ( model, Cmd.none )

                Just ( ProjectInfoContent cfprj, ( vista, ventana ) ) ->
                    let
                        ncfprj =
                            handleCFChange fid str cfprj

                        ncontent =
                            ProjectInfoContent ncfprj

                        nvista =
                            { vista | content = ncontent }

                        nvistas =
                            Dict.insert
                                ventana.vista
                                nvista
                                model.vistas

                        nprj =
                            case ncfprj of
                                ProjectCForm prj ->
                                    prj

                                -- This shouldn't happen.
                                _ ->
                                    projectFormData
                    in
                    ( { model | vistas = nvistas }
                    , if nprj.submitted then
                        sendMsg (FormSubmit tp)

                      else
                        Cmd.none
                    )

                Just ( GlobalSettingsContent cfglb, ( vista, ventana ) ) ->
                    let
                        ncfglb =
                            handleCFChange fid str cfglb

                        ncontent =
                            GlobalSettingsContent ncfglb

                        nvista =
                            { vista | content = ncontent }

                        nvistas =
                            Dict.insert
                                ventana.vista
                                nvista
                                model.vistas

                        nglb =
                            case ncfglb of
                                GlobalCForm glb ->
                                    glb

                                -- This shouldn't happen.
                                _ ->
                                    globalFormData
                    in
                    ( { model | vistas = nvistas }
                    , if nglb.submitted then
                        sendMsg (FormSubmit tp)

                      else
                        Cmd.none
                    )

                Just ( ImportOptionsContent cfimp, ( vista, ventana ) ) ->
                    let
                        ncfimp =
                            handleCFChange fid str cfimp

                        ncontent =
                            ImportOptionsContent ncfimp

                        nvista =
                            { vista | content = ncontent }

                        nvistas =
                            Dict.insert
                                ventana.vista
                                nvista
                                model.vistas

                        nimp =
                            case ncfimp of
                                ImportCForm imp ->
                                    imp

                                -- This shouldn't happen.
                                _ ->
                                    importFormData
                    in
                    ( { model | vistas = nvistas }
                    , if nimp.submitted then
                        sendMsg (FormSubmit tp)

                      else
                        Cmd.none
                    )

                Just ( NewDocContent cfint, ( vista, ventana ) ) ->
                    let
                        ncfint =
                            handleCFChange fid str cfint

                        ncontent =
                            NewDocContent ncfint

                        nvista =
                            { vista | content = ncontent }

                        nvistas =
                            Dict.insert
                                ventana.vista
                                nvista
                                model.vistas

                        nint =
                            case ncfint of
                                InterlinearCForm int ->
                                    int

                                -- This shouldn't happen.
                                _ ->
                                    Form.Interlinear.initData
                    in
                    ( { model | vistas = nvistas }
                    , if nint.submitted then
                        Cmd.batch
                            [ Task.perform SetTime Time.now
                            , sendMsg (FormSubmit tp)
                            ]

                      else
                        Cmd.none
                    )

                Just ( DocContent dc, ( vista, ventana ) ) ->
                    case dc.edit of
                        Nothing ->
                            ( model, Cmd.none )

                        Just cfint ->
                            let
                                ncfint =
                                    handleCFChange fid str cfint

                                nedit =
                                    Just ncfint

                                ncontent =
                                    DocContent { dc | edit = nedit }

                                nvista =
                                    { vista | content = ncontent }

                                nvistas =
                                    Dict.insert
                                        ventana.vista
                                        nvista
                                        model.vistas

                                nint =
                                    case ncfint of
                                        InterlinearCForm int ->
                                            int

                                        -- This shouldn't happen
                                        _ ->
                                            Form.Interlinear.initData
                            in
                            ( { model | vistas = nvistas }
                            , if nint.submitted then
                                Cmd.batch
                                    [ Task.perform SetTime Time.now
                                    , sendMsg (FormSubmit tp)
                                    ]

                              else
                                Cmd.none
                            )

                Just ( TranslationContent _, _ ) ->
                    ( model, Cmd.none )

                Just ( TranslationsContent _, _ ) ->
                    ( model, Cmd.none )

                Just ( InterlinearsContent _, _ ) ->
                    ( model, Cmd.none )


handleCFormSubmit : Maybe CForm -> Vista -> Model -> ( Model, Cmd Msg )
handleCFormSubmit edit vista model =
    case edit of
        Just (ProjectCForm prj) ->
            let
                jsonValue =
                    E.object
                        [ ( "identifier", E.string prj.identifier )
                        , ( "enabled", E.bool True )
                        , ( "title", E.string prj.title.value )
                        , ( "url", E.string prj.url.value )
                        ]

                nvista =
                    { vista
                        | content =
                            ProjectInfoContent (ProjectCForm projectFormData)
                    }

                nvistas =
                    Dict.insert nvista.identifier nvista model.vistas
            in
            ( { model | vistas = nvistas }
            , updateProject jsonValue
            )

        Just (GlobalCForm glb) ->
            let
                jsonValue =
                    E.object
                        [ ( "email", E.string glb.email.value )
                        , ( "name", E.string glb.name.value )
                        ]

                nvista =
                    { vista
                        | content =
                            GlobalSettingsContent (GlobalCForm globalFormData)
                    }

                nvistas =
                    Dict.insert nvista.identifier nvista model.vistas
            in
            ( { model | vistas = nvistas }
            , updateGlobalSettings jsonValue
            )

        Just (ImportCForm imp) ->
            let
                jsonValue =
                    E.object
                        [ ( "filepath", E.string imp.filepath )
                        , ( "kind", E.string imp.kind.value )
                        , ( "project", E.string imp.project.value )
                        ]

                nvista =
                    { vista
                        | content =
                            ImportOptionsContent (ImportCForm importFormData)
                    }

                nvistas =
                    Dict.insert nvista.identifier nvista model.vistas
            in
            ( { model | vistas = nvistas }, importFile jsonValue )

        Just (InterlinearCForm int) ->
            let
                nventanas =
                    model.ventanas
                        |> Dict.filter
                            (\_ v -> v.vista == vista.identifier)
                        |> Dict.map
                            (\_ v ->
                                let
                                    params =
                                        v.params
                                in
                                { v | params = { params | edit = False } }
                            )
                        |> (\vs -> Dict.union vs model.ventanas)

                ( uuid, seeds ) =
                    case int.id of
                        Nothing ->
                            UUID.step model.seeds

                        Just uuid_ ->
                            ( uuid_, model.seeds )

                -- This is set to change after refactor so a fake
                -- value is provided for Nothing.
                meId =
                    case model.me of
                        Nothing ->
                            ""

                        Just p ->
                            p.id

                interlinear : M.Interlinear
                interlinear =
                    { id = uuid
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
                        , docid = M.InterlinearId uuid
                        , time = model.time
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
                        vista.project
                    , address =
                        M.identifierToString
                            (M.MyDocId <|
                                M.InterlinearId uuid
                            )
                    , content =
                        [ M.MyInterlinear interlinear
                        , M.MyModification modification
                        ]
                            |> List.map M.encoder
                            |> E.list identity
                    }
            in
            ( { model | seeds = seeds, ventanas = nventanas }
            , Cmd.batch
                [ send (envelopeEncoder envelope) ]
            )

        Nothing ->
            ( model, Cmd.none )


handleCFChange : FieldId -> String -> CForm -> CForm
handleCFChange fid str data =
    case fid of
        ImportForm field ->
            handleCFImpChange field str data

        GlobalForm field ->
            handleCFGlbChange field str data

        ProjectForm field ->
            handleCFPrjChange field str data

        InterlinearForm field ->
            handleCFIntChange field str data


handleCFGlbChange : GlobalField -> String -> CForm -> CForm
handleCFGlbChange fid str cfglb =
    case cfglb of
        GlobalCForm glb ->
            handleCFGlbChange_ fid str glb |> GlobalCForm

        _ ->
            cfglb


handleCFPrjChange : ProjectField -> String -> CForm -> CForm
handleCFPrjChange fid str cfprj =
    case cfprj of
        ProjectCForm prj ->
            handleCFPrjChange_ fid str prj |> ProjectCForm

        _ ->
            cfprj


handleCFImpChange : ImportField -> String -> CForm -> CForm
handleCFImpChange fid str cfimp =
    case cfimp of
        ImportCForm imp ->
            handleCFImpChange_ fid str imp |> ImportCForm

        _ ->
            cfimp


handleCFGlbChange_ : GlobalField -> String -> GlobalFormData -> GlobalFormData
handleCFGlbChange_ fid str glb =
    let
        email =
            glb.email

        name =
            glb.name

        toperr =
            "Please correct form."

        valid glb_ =
            List.all identity
                [ glb_.email.valid
                , glb_.name.valid
                ]

        defGlb glb_ =
            let
                valid_ =
                    valid glb_
            in
            { glb_
                | changed = True
                , valid = valid_
                , error =
                    if valid_ then
                        ""

                    else
                        toperr
            }
    in
    case fid of
        GlbEmailF ->
            if String.isEmpty str then
                { glb
                    | email =
                        { email
                            | value = str
                            , valid = False
                            , error = "An email address is required."
                            , changed = True
                        }
                }
                    |> defGlb

            else
                case Email.fromString str of
                    Nothing ->
                        { glb
                            | email =
                                { email
                                    | value = str
                                    , valid = False
                                    , error = "Invalid email address."
                                    , changed = True
                                }
                        }
                            |> defGlb

                    Just _ ->
                        { glb
                            | email =
                                { email
                                    | value = str
                                    , valid = True
                                    , error = ""
                                    , changed = True
                                }
                        }
                            |> defGlb

        GlbNameF ->
            if String.isEmpty str then
                { glb
                    | name =
                        { name
                            | value = str
                            , valid = False
                            , error = "A name is required."
                            , changed = True
                        }
                }
                    |> defGlb

            else
                { glb
                    | name =
                        { name
                            | value = str
                            , valid = True
                            , error = ""
                            , changed = True
                        }
                }
                    |> defGlb

        GlbSaveF ->
            if defGlb glb |> .valid then
                { glb | submitted = True }

            else
                glb

        GlbCancelF ->
            globalFormData


handleCFPrjChange_ : ProjectField -> String -> ProjectFormData -> ProjectFormData
handleCFPrjChange_ fid str prj =
    let
        title =
            prj.title

        url =
            prj.url

        toperr =
            "Please correct form."

        valid prj_ =
            List.all identity
                [ prj_.title.valid
                , prj_.url.valid
                ]

        defPrj prj_ =
            let
                valid_ =
                    valid prj_
            in
            { prj_
                | changed = True
                , valid = valid_
                , error =
                    if valid_ then
                        ""

                    else
                        toperr
            }
    in
    case fid of
        PrjTitleF ->
            if String.isEmpty str then
                { prj
                    | title =
                        { title
                            | value = str
                            , valid = False
                            , error = "A title is required."
                            , changed = True
                        }
                }
                    |> defPrj

            else
                { prj
                    | title =
                        { title
                            | value = str
                            , valid = True
                            , error = ""
                            , changed = True
                        }
                }
                    |> defPrj

        PrjUrlF ->
            case ( String.isEmpty str, Url.fromString str ) of
                ( False, Nothing ) ->
                    { prj
                        | url =
                            { url
                                | value = str
                                , valid = False
                                , error = "A URL is not required, but it must be valid if provided."
                                , changed = True
                            }
                    }
                        |> defPrj

                _ ->
                    { prj
                        | url =
                            { url
                                | value = str
                                , valid = True
                                , error = ""
                                , changed = True
                            }
                    }
                        |> defPrj

        PrjSaveF ->
            if defPrj prj |> .valid then
                { prj | submitted = True }

            else
                prj

        PrjCancelF ->
            projectFormData


handleCFImpChange_ : ImportField -> String -> ImportFormData -> ImportFormData
handleCFImpChange_ fid str imp =
    let
        kind =
            imp.kind

        kindopts =
            List.map Tuple.second kind.options

        project =
            imp.project

        projopts =
            List.map Tuple.second project.options

        toperr =
            "Pleae correct form."

        valid imp_ =
            List.all identity
                [ imp_.kind.valid
                , imp_.project.valid
                ]

        defImp imp_ =
            let
                valid_ =
                    valid imp_
            in
            { imp_
                | changed = True
                , valid = valid_
                , error =
                    if valid_ then
                        ""

                    else
                        toperr
            }
    in
    case fid of
        ImpKindF ->
            if List.member str kindopts then
                { imp
                    | kind =
                        { kind
                            | value = str
                            , valid = True
                            , changed = True
                            , error = ""
                        }
                }
                    |> defImp

            else
                { imp
                    | kind =
                        { kind
                            | value = ""
                            , valid = False
                            , changed = True
                            , error = "Choose an import type."
                        }
                }
                    |> defImp

        ImpProjectF ->
            if List.member str projopts then
                { imp
                    | project =
                        { project
                            | value = str
                            , valid = True
                            , changed = True
                            , error = ""
                        }
                }
                    |> defImp

            else
                { imp
                    | project =
                        { project
                            | value = ""
                            , valid = False
                            , changed = True
                            , error = "Choose a project."
                        }
                }
                    |> defImp

        ImpImportF ->
            if defImp imp |> .valid then
                { imp | submitted = True }

            else
                imp

        ImpCancelF ->
            importFormData


handleCFIntChange : Form.Interlinear.Field -> String -> CForm -> CForm
handleCFIntChange fid str cfint =
    case cfint of
        InterlinearCForm int ->
            handleCFIntChange_ fid str int |> InterlinearCForm

        _ ->
            cfint


handleCFIntChange_ : Form.Interlinear.Field -> String -> Form.Interlinear.Data -> Form.Interlinear.Data
handleCFIntChange_ fid str int =
    Form.Interlinear.change fid str int
        

maybeInitForm : String -> Vistas -> Vistas
maybeInitForm vid oldvistas =
    case getContentVistaFromVistas vid oldvistas of
        Just ( DocContent dc, vista ) ->
            case ( dc.view.doc, dc.edit ) of
                ( Just (M.MyInterlinear int), Nothing ) ->
                    let
                        formData =
                            Form.Interlinear.init int
                                
                        ndc =
                            { view =
                                dc.view
                            , edit =
                                Just (InterlinearCForm formData)
                            }

                        newvista =
                            { vista | content = DocContent ndc }

                        newvistas =
                            Dict.insert vid newvista oldvistas
                    in
                    newvistas

                _ ->
                    oldvistas

        -- NewDocContent is specific to the Menkayonta notion of Doc.
        Just ( NewDocContent _, _ ) ->
            oldvistas

        Just ( TranslationContent _, _ ) ->
            oldvistas

        Just ( TranslationsContent _, _ ) ->
            oldvistas

        Just ( InterlinearsContent _, _ ) ->
            oldvistas

        Just ( ProjectInfoContent _, _ ) ->
            oldvistas

        Just ( ImportOptionsContent _, _ ) ->
            oldvistas

        Just ( GlobalSettingsContent _, _ ) ->
            oldvistas

        Nothing ->
            oldvistas


handleReceivedVista : E.Value -> String -> String -> Model -> ( Model, Cmd Msg )
handleReceivedVista val short full model =
    case D.decodeValue vistaDecoder val of
        Err err ->
            ( { model | error = D.errorToString err }
            , Cmd.none
            )

        Ok v ->
            handleVista v short full model


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
                    ( newmodel, sendMsg (NewTab vt) )

                Just tp ->
                    ( newmodel, sendMsg (FocusTab tp) )


{-| insertTabPath, newTabPath, and createNecessary are all helpers for
Move Direction. Each provides Direction specific code for some
aspect of the Move operation. This is for the case when movement
places the focused tab in a preexisting row with tabs.
-}
insertTabPath : Direction -> TabPath -> ( List Int, List Int ) -> List TabPath -> TabPath
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


{-| Use the counter (c) to provide new TabPaths that will be rendered
below, above, to the left or right of the focused tab.
-}
newTabPath : Direction -> TabPath -> Int -> TabPath
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
createNecessary : Direction -> TabPath -> ( List Int, List Int ) -> Bool
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
        , receivedPersonIndex ReceivedPersonIndex
        , receivedAllDoc ReceivedAllDoc
        , receivedDoc ReceivedDoc
        , newProject (\_ -> FormInit "" (ProjectCForm projectFormData))
        , importOptions ImportOptionsFileMenu
        , globalSettings GlobalSettingsMenu
        , moveLeft_ (\_ -> Move Left)
        , moveRight_ (\_ -> Move Right)
        , moveUp_ (\_ -> Move Up)
        , moveDown_ (\_ -> Move Down)
        , closeTab_ (\_ -> CloseTab)
        , cloneTab_ (\_ -> CloneTab)
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


viewTabHeader : Model -> TabPath -> Html.Html Msg
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
        [ Event.onClick (FocusTab tp)
        , Attr.id (tpToS tp)
        , Attr.classList
            [ ( "focused", focused )
            , ( "secondary", not focused && visible )
            , ( "secondary outline", not focused && not visible )
            , ( "tab-nav", True )
            ]
        ]
        [ Html.text ventana.title ]


viewTab : Model -> TabPath -> Html.Html Msg
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


viewTabListItem : TabPath -> Ventana -> Html.Html Msg
viewTabListItem tp ventana =
    Html.li []
        [ Html.a
            [ Attr.href "#"
            , Event.onClick <| GotoTab tp
            , Attr.class "secondary"
            ]
            [ Html.text ventana.fullTitle ]
        ]


viewLoadingProject : Model -> ProjectInfo -> Html.Html Msg
viewLoadingProject model p =
    Html.span
        [ Attr.attribute "aria-busy"
            (if Set.member p.identifier model.loading then
                "true"

             else
                "false"
            )
        ]
        [ Html.text p.title ]


viewProject : Model -> ProjectInfo -> Html.Html Msg
viewProject model p =
    Html.li []
        [ viewLoadingProject model p
        , Html.ul []
            [ Html.li []
                [ Html.a
                    [ Attr.href "#"
                    , Event.onClick <|
                        FormInit p.identifier <|
                            ProjectCForm
                                { projectFormData
                                    | identifier = p.identifier
                                    , title = { blankString | value = p.title }
                                    , url = { blankString | value = Maybe.withDefault "" p.url }
                                }
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
                        FormInit p.identifier <|
                            InterlinearCForm Form.Interlinear.initData
                    , Attr.class "secondary"
                    ]
                    [ Html.text "New Gloss" ]
                ]
            ]
        ]


viewVista : Model -> TabPath -> Vista -> Html Msg
viewVista model tp vista =
    case vista.content of
        ProjectInfoContent (ProjectCForm prj) ->
            viewDocContentEditVista tp (ProjectCForm prj)

        ProjectInfoContent _ ->
            Html.text "no such form"

        GlobalSettingsContent (GlobalCForm glb) ->
            viewDocContentEditVista tp (GlobalCForm glb)

        GlobalSettingsContent _ ->
            Html.text "no such form"

        TranslationContent trn ->
            viewTranslation model trn

        TranslationsContent trns ->
            let
                params =
                    Dict.get tp model.ventanas
                        |> Maybe.map .params
                        |> Maybe.withDefault { defVParams | length = 20 }

                ss =
                    params.searchString

                searched =
                    List.filter
                        (\t ->
                            String.contains ss t.key
                                || String.contains ss t.value
                        )
                        trns

                ts =
                    List.take params.length searched

                len =
                    String.fromInt params.length
            in
            Html.div []
                [ Html.label []
                    [ Html.text <| "Show (" ++ len ++ ")"
                    , Html.input
                        [ Attr.type_ "text"
                        , Attr.value len
                        , Event.onInput (ChangeLengthParam tp)
                        ]
                        []
                    , Html.input
                        [ Attr.type_ "search"
                        , Attr.name "search"
                        , Attr.placeholder "Search"
                        , Attr.attribute "aria-label" "Search"
                        , Attr.value ss
                        , Event.onInput (ChangeSearchParam tp)
                        ]
                        []
                    ]
                , Html.table [] (List.map (viewTranslation model) ts)
                ]

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

        DocContent od ->
            viewDocContentVista
                { vista = vista
                , tp = tp
                , od = od
                , model = model
                }

        NewDocContent formData ->
            viewNewDocContentVista
                { vista = vista
                , tp = tp
                , fd = formData
                , model = model
                }

        ImportOptionsContent (ImportCForm imp) ->
            viewDocContentEditVista tp (ImportCForm imp)

        ImportOptionsContent _ ->
            Html.text "no such form"


viewNewDocContentVista :
    { vista : Vista
    , tp : TabPath
    , fd : CForm
    , model : Model
    }
    -> Html.Html Msg
viewNewDocContentVista { vista, tp, fd, model } =
    viewDocContentEditVista tp fd


viewDocContentVista :
    { vista : Vista
    , tp : TabPath
    , od : { view : M.OneDoc, edit : Maybe CForm }
    , model : Model
    }
    -> Html.Html Msg
viewDocContentVista { vista, tp, od, model } =
    let
        params =
            Dict.get tp model.ventanas
                |> Maybe.map .params
                |> Maybe.withDefault defVParams
    in
    case od.view.doc of
        Just (M.MyInterlinear int) ->
            Html.div []
                [ Html.nav []
                    [ Html.ul []
                        [ Html.li []
                            [ Html.a
                                [ Attr.href "#"
                                , Event.onClick (ChangeEditParam tp)
                                ]
                                [ Html.text
                                    (if params.edit then
                                        "View"

                                     else
                                        "Edit"
                                    )
                                ]
                            ]
                        ]
                    ]
                , if params.edit then
                    case od.edit of
                        Just cform ->
                            viewDocContentEditVista tp cform

                        Nothing ->
                            Html.text "Form not initialized"

                  else
                    viewDocContentViewVista
                        { vista = vista
                        , od = od.view
                        , int = int
                        }
                ]

        _ ->
            Html.div [] [ Html.text "doc not supported" ]


viewDocContentEditVista : TabPath -> CForm -> Html.Html Msg
viewDocContentEditVista tp cform =
    case cform of
        InterlinearCForm int ->
            if int.submitted then
                Html.span
                    [ Attr.attribute "aria-busy" "true" ]
                    [ Html.text "Saving changes." ]

            else
                viewCFInterlinearVista tp int

        ImportCForm imp ->
            if imp.submitted then
                Html.span
                    [ Attr.attribute "aria-busy" "true" ]
                    [ Html.text "Saving changes." ]

            else
                viewCFImportVista tp imp

        GlobalCForm glb ->
            if glb.submitted then
                Html.span
                    [ Attr.attribute "aria-busy" "true" ]
                    [ Html.text "Saving changes." ]

            else
                viewCFGlobalVista tp glb

        ProjectCForm prj ->
            if prj.submitted then
                -- Normally, the form is closed immediately after
                -- submitting.
                Html.span
                    [ Attr.attribute "aria-busy" "true" ]
                    [ Html.text "Saving changes." ]

            else
                viewCFProjectVista tp prj


type alias FieldDescription =
    { formname : String
    , label : String
    , kind :
        List (Html.Attribute Msg)
        -> List (Html.Html Msg)
        -> Html.Html Msg
    , oninput : String -> Msg
    , name : String
    , value : String
    , original : String
    , changed : Bool
    , valid : Bool
    , help : String
    , error : String
    , disabled : Bool
    , deleted : Bool
    , spellcheck : Bool
    , options : List ( String, String )
    , id : Maybe Int
    }


viewCField : FieldDescription -> Html.Html Msg
viewCField fd =
    let
        id =
            Maybe.withDefault -1 fd.id

        name =
            [ fd.formname
            , fd.name
            ]
                |> String.join "-"

        helper =
            [ name
            , String.fromInt id
            , "helper"
            ]
                |> String.join "-"
    in
    Html.label []
        [ Html.a
            [ Attr.class "secondary"
            , Attr.attribute "data-tooltip" "Reload Field"
            , Attr.attribute "data-placement" "right"
            , Attr.href "#"
            , Event.onClick (fd.oninput fd.original)
            ]
            [ Html.text "🗘 " ]
        , Html.text fd.label
        , fd.kind
            [ Event.onInput fd.oninput
            , Attr.name name
            , Attr.value fd.value
            , Attr.attribute "aria-label" fd.label
            , Attr.attribute "aria-describedby" helper
            , Attr.spellcheck fd.spellcheck
            , if fd.changed then
                isInValidAttr fd.valid

              else
                Attr.class "unchanged-field"
            , Attr.disabled (fd.disabled || fd.deleted)
            ]
            []
        , Html.small
            [ Attr.id helper ]
            [ if fd.deleted then
                Html.text "This content will be removed."

              else if fd.valid then
                Html.text fd.help

              else
                Html.text fd.error
            ]
        ]


viewCSelectField : FieldDescription -> Html.Html Msg
viewCSelectField fd =
    let
        id =
            Maybe.withDefault -1 fd.id

        name =
            [ fd.formname
            , fd.name
            ]
                |> String.join "-"

        helper =
            [ name
            , String.fromInt id
            , "helper"
            ]
                |> String.join "-"
    in
    Html.label []
        [ Html.a
            [ Attr.class "secondary"
            , Attr.attribute "data-tooltip" "Reload Field"
            , Attr.attribute "data-placement" "right"
            , Attr.href "#"
            , Event.onClick (fd.oninput fd.original)
            ]
            [ Html.text "🗘 " ]
        , Html.text fd.label
        , fd.kind
            [ Event.onInput fd.oninput
            , Attr.name name
            , Attr.value fd.value
            , Attr.attribute "aria-label" fd.label
            , Attr.attribute "aria-describedby" helper
            , Attr.spellcheck fd.spellcheck
            , if fd.changed then
                isInValidAttr fd.valid

              else
                Attr.class "unchanged-field"
            , Attr.disabled (fd.disabled || fd.deleted)
            ]
            (List.map
                (\opt ->
                    Html.option
                        [ Attr.value (Tuple.second opt) ]
                        [ Html.text (Tuple.first opt) ]
                )
                (( "", "" ) :: fd.options)
            )
        , Html.small
            [ Attr.id helper ]
            [ if fd.deleted then
                Html.text "This content will be removed."

              else if fd.valid then
                Html.text fd.help

              else
                Html.text fd.error
            ]
        ]


viewCFInterlinearVista : TabPath -> Form.Interlinear.Data -> Html.Html Msg
viewCFInterlinearVista tp int =
    Form.Interlinear.display int (\field -> FormChange tp (InterlinearForm field))


viewCFGlobalVista : TabPath -> GlobalFormData -> Html.Html Msg
viewCFGlobalVista tp glb =
    Html.form []
        [ viewCField
            { formname = "globalsettings"
            , label = "Email Address"
            , kind = Html.input
            , oninput = FormChange tp (GlobalForm GlbEmailF)
            , name = "email"
            , value = glb.email.value
            , original = glb.email.original
            , changed = glb.email.changed
            , valid = glb.email.valid
            , help = "Your email address."
            , error = glb.email.error
            , disabled = False
            , deleted = False
            , spellcheck = False
            , options = []
            , id = Nothing
            }
        , viewCField
            { formname = "globalsettings"
            , label = "Name"
            , kind = Html.input
            , oninput = FormChange tp (GlobalForm GlbNameF)
            , name = "name"
            , value = glb.name.value
            , original = glb.name.original
            , changed = glb.name.changed
            , valid = glb.name.valid
            , help = "Your name."
            , error = glb.name.error
            , disabled = False
            , deleted = False
            , spellcheck = False
            , options = []
            , id = Nothing
            }
        , Html.button
            (if glb.valid then
                [ Event.onClick <|
                    FormChange tp (GlobalForm GlbSaveF) ""
                , Attr.type_ "button"
                ]

             else
                [ Attr.attribute "data-tooltip" glb.error
                , Attr.attribute "data-placement" "right"
                , Attr.type_ "button"
                , Event.onClick None
                ]
            )
            [ Html.text "Save" ]
        , Html.button
            [ Attr.class "secondary"
            , Attr.type_ "button"
            , Event.onClick <|
                MultiMsg
                    [ FormChange tp (GlobalForm GlbCancelF) ""
                    , CloseTab
                    ]
            ]
            [ Html.text "Cancel" ]
        ]


viewCFProjectVista : TabPath -> ProjectFormData -> Html.Html Msg
viewCFProjectVista tp prj =
    Html.form []
        [ viewCField
            { formname = "projectconf"
            , label = "Identifier"
            , kind = Html.input
            , oninput = \_ -> None
            , name = "identifier"
            , value = prj.identifier
            , original = prj.identifier
            , changed = False
            , valid = True
            , help = "A unique identifier for the project."
            , error = ""
            , disabled = True
            , deleted = False
            , spellcheck = False
            , options = []
            , id = Nothing
            }
        , viewCField
            { formname = "projectconf"
            , label = "Project Title"
            , kind = Html.input
            , oninput = FormChange tp (ProjectForm PrjTitleF)
            , name = "title"
            , value = prj.title.value
            , original = prj.title.original
            , changed = prj.title.changed
            , valid = prj.title.valid
            , help = "A short title for the project."
            , error = prj.title.error
            , disabled = False
            , deleted = False
            , spellcheck = True
            , options = []
            , id = Nothing
            }
        , viewCField
            { formname = "projectcof"
            , label = "Server Url"
            , kind = Html.input
            , oninput = FormChange tp (ProjectForm PrjUrlF)
            , name = "url"
            , value = prj.url.value
            , original = prj.url.original
            , changed = prj.url.changed
            , valid = prj.url.valid
            , help = "A URL to a server to work with others."
            , error = prj.url.error
            , disabled = False
            , deleted = False
            , spellcheck = False
            , options = []
            , id = Nothing
            }
        , Html.button
            (if prj.valid then
                [ Event.onClick <|
                    FormChange tp (ProjectForm PrjSaveF) ""
                , Attr.type_ "button"
                ]

             else
                [ Attr.attribute "data-tooltip" prj.error
                , Attr.attribute "data-placement" "right"
                , Attr.type_ "button"
                , Event.onClick None
                ]
            )
            [ Html.text "Save" ]
        , Html.button
            [ Attr.class "secondary"
            , Attr.type_ "button"
            , Event.onClick <|
                MultiMsg
                    [ FormChange tp (ProjectForm PrjCancelF) ""
                    , CloseTab
                    ]
            ]
            [ Html.text "Cancel" ]
        ]


viewCFImportVista : TabPath -> ImportFormData -> Html.Html Msg
viewCFImportVista tp imp =
    Html.form []
        [ Html.fieldset []
            [ viewCField
                { formname = "importoptions"
                , label = "File Path"
                , kind = Html.input
                , oninput = \_ -> None
                , name = "filepath"
                , value = imp.filepath
                , original = imp.filepath
                , changed = False
                , valid = True
                , help = "The file chosen to import."
                , error = ""
                , disabled = True
                , deleted = False
                , spellcheck = False
                , options = []
                , id = Nothing
                }
            , viewCSelectField
                { formname = "importoptions"
                , label = "Import Type"
                , kind = Html.select
                , oninput = FormChange tp (ImportForm ImpKindF)
                , name = "kind"
                , value = imp.kind.value
                , original = imp.kind.original
                , changed = imp.kind.changed
                , valid = imp.kind.valid
                , help = "The format of the import file."
                , error = imp.kind.error
                , disabled = False
                , deleted = False
                , spellcheck = False
                , options = imp.kind.options
                , id = Nothing
                }
            , viewCSelectField
                { formname = "importoptions"
                , label = "Project"
                , kind = Html.select
                , oninput = FormChange tp (ImportForm ImpProjectF)
                , name = "project"
                , value = imp.project.value
                , original = imp.project.original
                , changed = imp.project.changed
                , valid = imp.project.valid
                , help = "The project that receives the import."
                , error = imp.project.error
                , disabled = False
                , deleted = False
                , spellcheck = False
                , options = imp.project.options
                , id = Nothing
                }
            ]
        , Html.button
            (if imp.valid then
                [ Event.onClick <|
                    FormChange tp (ImportForm ImpImportF) ""
                , Attr.type_ "button"
                ]

             else
                [ Attr.attribute "data-tooltip" imp.error
                , Attr.attribute "data-placement" "right"
                , Attr.type_ "button"
                , Event.onClick None
                ]
            )
            [ Html.text "Import" ]
        , Html.button
            [ Attr.class "secondary"
            , Attr.type_ "button"
            , Event.onClick <|
                MultiMsg
                    [ FormChange tp (ImportForm ImpCancelF) ""
                    , CloseTab
                    ]
            ]
            [ Html.text "Cancel" ]
        ]


viewDocContentViewVista :
    { vista : Vista
    , od : M.OneDoc
    , int : M.Interlinear
    }
    -> Html.Html Msg
viewDocContentViewVista { vista, od, int } =
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


viewError : Model -> Error -> Html Msg
viewError _ err =
    Html.text err.message


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
                viewAnn int.text int.ann.breaks int.ann.glosses

            else
                Html.p [] [ Html.text int.text ]

        transLines =
            List.map (\t -> Html.p [] [ Html.text t.translation ]) (Dict.values int.translations)
    in
    Html.div [] (srcLine :: transLines)


viewAnn : String -> String -> String -> Html.Html Msg
viewAnn src brk gls =
    let
        src_ =
            String.split " " src

        brk_ =
            String.split " " brk

        gls_ =
            String.split " " gls

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


viewTranslation : Model -> Translation -> Html.Html Msg
viewTranslation _ dc =
    Html.tr []
        [ Html.td [] [ Html.text dc.key ]
        , Html.td [] [ Html.text dc.value ]
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
closeTab : Bool -> TabPath -> Model -> Model
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
reassign : TabPath -> TabPath -> Model -> Model
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


tpToS : TabPath -> String
tpToS ( c, ( r, t ) ) =
    [ c, r, t ]
        |> List.map String.fromInt
        |> String.join ","


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
        "all-translations" ->
            D.map TranslationsContent
                (D.field "content" translationsDecoder)

        -- "new-project" ->
        --     D.map ProjectInfoContent
        --         (D.field "content" projectInfoDecoder)
        "all-interlinears" ->
            D.map InterlinearsContent
                (D.field "content" <| D.list interlinearDecoder)

        _ ->
            D.fail ("Unsupported content kind " ++ kind)


globalConfigDecoder : D.Decoder GlobalConfig
globalConfigDecoder =
    D.map3 GlobalConfig
        (D.field "projects" <| D.list projectInfoDecoder)
        (D.field "name" <| D.nullable D.string)
        (D.field "email" <| D.nullable D.string)


projectInfoDecoder : D.Decoder ProjectInfo
projectInfoDecoder =
    D.map3 ProjectInfo
        (D.field "title" D.string)
        (D.field "identifier" D.string)
        (D.field "url" (D.nullable D.string))


getVistaVentana : TabPath -> Model -> Maybe ( Vista, Ventana )
getVistaVentana tp model =
    Dict.get tp model.ventanas
        |> Maybe.andThen
            (\ventana ->
                Dict.get ventana.vista model.vistas
                    |> Maybe.map (\vista -> ( vista, ventana ))
            )


getContentVistaVentana : TabPath -> Model -> Maybe ( Content, ( Vista, Ventana ) )
getContentVistaVentana tp model =
    getVistaVentana tp model
        |> Maybe.map (\( vis, ven ) -> ( vis.content, ( vis, ven ) ))


getContentVistaFromVistas : String -> Vistas -> Maybe ( Content, Vista )
getContentVistaFromVistas vid vistas =
    Dict.get vid vistas
        |> Maybe.map (\vista -> ( vista.content, vista ))


getByVista : String -> Dict TabPath Ventana -> Maybe TabPath
getByVista vista ventanas =
    List.head <| getAllByVista vista ventanas


getAllByVista : String -> Dict TabPath Ventana -> List TabPath
getAllByVista vista ventanas =
    -- TODO use Dict.filter (this may not warrant being a helper
    -- function.)
    List.filter (\( _, v ) -> v.vista == vista) (Dict.toList ventanas)
        |> List.map Tuple.first


getProjectTitle : String -> Model -> Maybe String
getProjectTitle projid model =
    model.gconfig
        |> Maybe.map .projects
        |> Maybe.andThen (LE.find (\x -> x.identifier == projid))
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


port requestDocId : E.Value -> Cmd msg


port requestAllDocId : E.Value -> Cmd msg


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


port receivedPersonIndex : (E.Value -> msg) -> Sub msg


port receivedAllDoc : (E.Value -> msg) -> Sub msg


port receivedDoc : (E.Value -> msg) -> Sub msg


{-| The "New Project" menu item was clicked.
-}
port newProject : (() -> msg) -> Sub msg


{-| The "New Project" menu item was clicked.
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
