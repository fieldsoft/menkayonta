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
import SharedTypes as ST
import Menkayonta as M exposing (Interlinear)
import Task
import UUID exposing (UUID)


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


port requestDocId : E.Value -> Cmd msg


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


{-| The project index is a listing of all translated items with their
translations, which serves as an entry point to a project. This port
indicates that the index was received.
-}
port receivedProjectIndex : (E.Value -> msg) -> Sub msg


port receivedInterlinearIndex : (E.Value -> msg) -> Sub msg


port receivedDoc : (E.Value -> msg) -> Sub msg


{-| The "New Project" menu item was clicked.
-}
port newProject : (String -> msg) -> Sub msg


{-| The "New Project" menu item was clicked.
-}
port globalSettings : (E.Value -> msg) -> Sub msg


{-| The "Import File" menu item was clicked.
-}
port importOptions : (String -> msg) -> Sub msg


{-| Send ProjectInfo to the backend for creation.
-}
port createProject : E.Value -> Cmd msg


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

                 
{-| A Ventana supplies the title and a referrence to a Vista, which is
an identifier for some viewable content. I use Spanish when there are
already commonly referred to object or concepts such as "window" or
"view".
-}
type alias Ventana =
    { title : String
    , vista : String
    , params : VentanaParams
    }


type alias VentanaParams =
    { length : Int
    , searchString : String
    , edit : Bool
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
    { projects : List ProjectInfo
    , name : Maybe String
    , email : Maybe String
    }


{-| This is the subset of global configuration that is not specific to
a project.
-}
type alias GlobalSettings =
    { name : String
    , email : String
    }


type alias ProjectInfo =
    { title : String
    , identifier : String
    , enabled : Bool
    , url : Maybe String
    }


type alias ImportOptions =
    { filepath : Maybe String
    , content : Maybe String
    , kind : String
    , project : String
    }


type alias Error =
    { message : String }


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


docRecDecode : D.Decoder DocRec
docRecDecode =
    D.map3 DocRec
        (D.field "command" D.string)
        (D.field "identifier" D.string)
        (D.field "doc" M.interlinearDecoder
        |> D.map InterlinearContent)


type alias FormData =
    { fields : Field FieldKind
    , submitted : Bool
    }


type alias Model =
    { gconfig : Maybe GlobalConfig
    , counter : Int
    , ventanas : Ventanas
    , focused : Maybe TabPath
    , visVentanas : VisVentanas
    , vistas : Vistas
    , windowHeight : Int
    , error : Maybe Error
    , forms : Dict String FormData
    , loading : Set String
    }


type Direction
    = Left
    | Right
    | Up
    | Down


type FieldKind
    = ProjectIdentifier
    | ProjectTitle
    | ProjectEnabled
    | ProjectUrl
    | ImportKind
    | ImportProject
    | ImportFile
    | ImportContent
    | GlobalName
    | GlobalEmail


type Msg
    = NewTab Ventana
    | FocusTab TabPath
    | CloseTab
    | CloneTab
    | Move Direction
    | SetWindowTitle String
    | ReceivedGlobalConfig E.Value
    | ReceivedProjectIndex E.Value
    | ReceivedInterlinearIndex E.Value
    | ReceivedDoc E.Value
    | RequestProjectIndex String
    | RequestInterlinearIndex String
    | RequestDocId String String
    | NewProjectMenu String
    | ImportOptionsFileMenu String
    | GlobalSettingsMenu E.Value
    | ProjectInfoFormChange TabPath (Field.Msg FieldKind)
    | ImportOptionsFormChange (Field.Msg FieldKind)
    | GlobalSettingsFormChange (Field.Msg FieldKind)
    | ProjectSettingsEdit ProjectInfo
    | FormSubmit TabPath
    | ChangeLengthParam TabPath String
    | ChangeSearchParam TabPath String
    | ChangeEditParam TabPath


type alias Flags =
    { windowHeight : Int }


type Content
    = TranslationContent Translation
    | TranslationsContent (List Translation)
    | InterlinearsContent (List Interlinear)
    | InterlinearContent M.Interlinear
    | ProjectInfoContent ProjectInfo
    | ImportOptionsContent ImportOptions
    | GlobalSettingsContent GlobalSettings
    | ErrorContent Error


type alias Translation =
    { key : String
    , value : String
    , id : String
    }


{-| These are not specific to any project and are kept around, even
when not in use.
-}
globalVistas : Dict String Vista
globalVistas =
    [ ( "new-project"
      , { project = "global"
        , kind = "new-project"
        , identifier = "new-project"
        , content = ProjectInfoContent (ProjectInfo "" "" True Nothing)
        }
      )
    , ( "import-options"
      , { project = "global"
        , kind = "import-options"
        , identifier = "import-options"
        , content = ImportOptionsContent (ImportOptions Nothing Nothing "" "")
        }
      )
    , ( "global-settings"
      , { project = "global"
        , kind = "global-settings"
        , identifier = "global-settings"
        , content = GlobalSettingsContent (GlobalSettings "" "")
        }
      )
    ]
        |> Dict.fromList


projectFields : ProjectInfo -> Field FieldKind
projectFields pi =
    Field.group []
        [ Field.text
            [ Field.label "Identifier"
            , Field.required True
            , Field.disabled True
            , Field.identifier ProjectIdentifier
            , Field.name "identifier"
            , Field.value (Value.string pi.identifier)
            ]
        , Field.text
            [ Field.label "Title"
            , Field.required True
            , Field.identifier ProjectTitle
            , Field.name "title"
            , Field.value (Value.string pi.title)
            ]
        , Field.checkbox
            [ Field.label "Enable"
            , Field.identifier ProjectEnabled
            , Field.name "enabled"
            , Field.hint "Be careful! Disabling a project will hide it."
            , Field.value (Value.bool pi.enabled)
            ]
        , Field.text
            [ Field.label "Url"
            , Field.required False
            , Field.identifier ProjectUrl
            , Field.name "url"
            , Field.value (Value.string <| Maybe.withDefault "" pi.url)
            ]
        ]


importOptionsFields : Maybe GlobalConfig -> Maybe String -> Field FieldKind
importOptionsFields gc filesource =
    let
        inputSource =
            case filesource of
                Nothing ->
                    Field.textarea
                        [ Field.label "Content"
                        , Field.required True
                        , Field.identifier ImportContent
                        , Field.name "content"
                        ]

                Just filepath ->
                    Field.text
                        [ Field.label "File"
                        , Field.required True
                        , Field.disabled True
                        , Field.identifier ImportFile
                        , Field.name "filepath"
                        , Field.value (Value.string filepath)
                        ]

        projects =
            case gc of
                Nothing ->
                    []

                Just gconf ->
                    gconf.projects
                        |> List.map
                            (\x -> ( x.title, Value.string x.identifier ))
    in
    Field.group []
        [ Field.select
            [ Field.label "Target Project"
            , Field.required True
            , Field.identifier ImportProject
            , Field.name "project"
            , Field.options projects
            ]
        , Field.select
            [ Field.label "Import Type"
            , Field.required True
            , Field.identifier ImportKind
            , Field.name "kind"
            , Field.stringOptions [ "Dative Form Json" ]
            ]
        , inputSource
        ]


globalSettingsFields : GlobalSettings -> Field FieldKind
globalSettingsFields gs =
    Field.group []
        [ Field.text
            [ Field.label "Your Name"
            , Field.required True
            , Field.identifier GlobalName
            , Field.name "name"
            , Field.value (Value.string gs.name)
            ]
        , Field.email
            [ Field.label "Your Email"
            , Field.required True
            , Field.identifier GlobalEmail
            , Field.name "email"
            , Field.value (Value.string gs.email)
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


defVParams =
    { length = 0
    , searchString = ""
    , edit = False
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        newProjectForm =
            { fields = projectFields <| ProjectInfo "" "" True Nothing
            , submitted = False
            }

        importForm =
            { fields = importOptionsFields Nothing Nothing
            , submitted = False
            }

        globalForm =
            { fields = globalSettingsFields <| GlobalSettings "" ""
            , submitted = False
            }

        forms =
            Dict.empty
                |> Dict.insert "new-project" newProjectForm
                |> Dict.insert "import-options" importForm
                |> Dict.insert "global-settings" globalForm
    in
    ( { gconfig = Nothing
      , counter = 0
      , ventanas = Dict.empty
      , focused = Nothing
      , visVentanas = Dict.empty
      , vistas = globalVistas
      , windowHeight = flags.windowHeight
      , error = Nothing
      , forms = forms
      , loading = Set.empty
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
                    ( newmodel, tp ) =
                        case model.focused of
                            Nothing ->
                                ( model, tabpath -1 -1 -1 )

                            Just tp1 ->
                                let
                                    ( column, ( row, _ ) ) =
                                        tp1

                                    tp2 =
                                        tabpath column row c
                                in
                                ( { model
                                    | counter = c + 1
                                    , ventanas = Dict.insert tp2 ventana model.ventanas
                                    , visVentanas = visInsert tp2 model.visVentanas
                                    , focused = Just tp2
                                  }
                                , tp2
                                )
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
                    ( { model | error = Just (Error (D.errorToString err)) }
                    , Cmd.none
                    )

                Ok gc1 ->
                    let
                        nm =
                            { model | gconfig = Just gc1 }

                        openMenu =
                            update (GlobalSettingsMenu gc) nm

                        cmd =
                            case ( gc1.name, gc1.email ) of
                                ( Nothing, _ ) ->
                                    openMenu

                                ( _, Nothing ) ->
                                    openMenu

                                _ ->
                                    ( nm, Cmd.none )
                    in
                    cmd

        RequestProjectIndex id ->
            ( { model | loading = Set.insert id model.loading }
            , requestProjectIndex id
            )

        RequestInterlinearIndex id ->
            ( { model | loading = Set.insert id model.loading }
            , requestInterlinearIndex id
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

        ReceivedProjectIndex pi ->
            handleReceivedVista pi "Index" model

        ReceivedInterlinearIndex ii ->
            handleReceivedVista ii "Glosses" model

        ReceivedDoc val ->
            let
                docRec =
                    D.decodeValue docRecDecode val

                data =
                    Result.map (\dr -> ( dr.identifier, dr.doc )) docRec
            in
            case data of
                Ok ( project, InterlinearContent i ) ->
                    let
                        st =
                            if String.length i.text > 5 then
                                "Gloss: "
                                    ++ String.left 5 i.text
                                    ++ "..."

                            else
                                "Gloss: " ++ i.text

                        vista =
                            { project = project
                            , kind = "interlinear"
                            , identifier = UUID.toString i.id
                            , content = InterlinearContent i
                            }
                    in
                    handleVista vista st model

                _ ->
                    ( model, Cmd.none )

        -- Open or focus the New Project form.
        NewProjectMenu ident ->
            let
                pi =
                    ProjectInfo "" ident True Nothing

                pif =
                    { fields = projectFields pi, submitted = False }

                forms =
                    Dict.insert "new-project" pif model.forms

                newVentana =
                    Ventana "New Project" "new-project" defVParams

                newmodel =
                    { model | forms = forms }
            in
            case getByVista "new-project" model.ventanas of
                Nothing ->
                    update (NewTab newVentana) newmodel

                Just tp ->
                    update (FocusTab tp) newmodel

        ProjectSettingsEdit pi ->
            let
                piid =
                    "edit-project::" ++ pi.identifier

                pif =
                    { fields = projectFields pi, submitted = False }

                tabtitle =
                    pi.title ++ " Settings"

                newVentana =
                    Ventana tabtitle piid defVParams

                newVista =
                    { project = pi.identifier
                    , kind = "edit-project"
                    , identifier = piid
                    , content = ProjectInfoContent pi
                    }

                vistas =
                    Dict.insert piid newVista model.vistas

                forms =
                    case Dict.get piid model.forms of
                        Nothing ->
                            Dict.insert piid pif model.forms

                        _ ->
                            model.forms

                newmodel =
                    { model | forms = forms, vistas = vistas }
            in
            case getByVista piid model.ventanas of
                Nothing ->
                    update (NewTab newVentana) newmodel

                Just tp ->
                    update (FocusTab tp) newmodel

        -- Open or focus the Import Options form with a filename.
        ImportOptionsFileMenu filepath ->
            let
                formData =
                    { fields =
                        importOptionsFields model.gconfig (Just filepath)
                    , submitted =
                        False
                    }

                forms =
                    Dict.insert "import-options" formData model.forms

                newmodel =
                    { model | forms = forms }
            in
            case getByVista "import-options" model.ventanas of
                Nothing ->
                    let
                        newVentana =
                            { title = "Import Options"
                            , vista = "import-options"
                            , params = defVParams
                            }
                    in
                    update (NewTab newVentana) newmodel

                Just tp ->
                    update (FocusTab tp) newmodel

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

                        gsForm =
                            { fields = globalSettingsFields gs
                            , submitted = False
                            }

                        forms =
                            Dict.insert "global-settings"
                                gsForm
                                model.forms

                        newmodel =
                            { model
                                | gconfig = Just gf
                                , forms = forms
                            }
                    in
                    case getByVista "global-settings" model.ventanas of
                        Nothing ->
                            let
                                newVentana =
                                    { title = "Settings"
                                    , vista = "global-settings"
                                    , params = defVParams
                                    }
                            in
                            update (NewTab newVentana) newmodel

                        Just tp ->
                            update (FocusTab tp) newmodel

        ProjectInfoFormChange tp mesg ->
            let
                ident =
                    Dict.get tp model.ventanas
                        |> Maybe.map .vista
                        |> Maybe.withDefault "bad-identifier"
            in
            case Dict.get ident model.forms of
                Just fd ->
                    let
                        ( fields, _ ) =
                            FParse.parseUpdate projectParser mesg fd.fields

                        pif =
                            { fd | fields = fields }

                        forms =
                            Dict.insert ident pif model.forms
                    in
                    ( { model | forms = forms }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ImportOptionsFormChange mesg ->
            case Dict.get "import-options" model.forms of
                Just fd ->
                    let
                        ( fields, _ ) =
                            FParse.parseUpdate importParser mesg fd.fields

                        ipf =
                            { fd | fields = fields }

                        forms =
                            Dict.insert "import-options" ipf model.forms
                    in
                    ( { model | forms = forms }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        GlobalSettingsFormChange mesg ->
            case Dict.get "global-settings" model.forms of
                Just fd ->
                    let
                        ( fields, _ ) =
                            FParse.parseUpdate globalParser mesg fd.fields

                        gsf =
                            { fd | fields = fields }

                        forms =
                            Dict.insert "global-settings" gsf model.forms
                    in
                    ( { model | forms = forms }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        FormSubmit tp ->
            let
                formid =
                    Dict.get tp model.ventanas |> Maybe.map .vista
            in
            case formid of
                Just ident ->
                    case Dict.get ident model.forms of
                        Nothing ->
                            ( model, Cmd.none )

                        Just fd ->
                            if fd.submitted then
                                -- The form was already
                                -- submitted. Just close the tab.
                                ( closeTab True tp model, Cmd.none )

                            else
                                let
                                    divid =
                                        String.split "::" ident

                                    ( nm, cmd ) =
                                        handleSubmit divid fd model
                                in
                                ( closeTab True tp model, cmd )

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

                        nv =
                            { ventana
                                | params =
                                    { params | edit = not params.edit }
                            }

                        nvs =
                            Dict.insert tp nv model.ventanas
                    in
                    ( { model | ventanas = nvs }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


handleReceivedVista : E.Value -> String -> Model -> ( Model, Cmd Msg )
handleReceivedVista val st model =
    case D.decodeValue vistaDecoder val of
        Err err ->
            ( { model | error = Just (Error (D.errorToString err)) }
            , Cmd.none
            )

        Ok v ->
            handleVista v st model


handleVista : Vista -> String -> Model -> ( Model, Cmd Msg )
handleVista vista st model =
    case getProjectTitle vista.project model of
        Nothing ->
            ( { model | error = Just (Error "No such project") }
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
                            { title = title ++ ": " ++ st
                            , vista = vista.identifier
                            , params = { defVParams | length = 20 }
                            }
                    in
                    update (NewTab vt) newmodel

                Just tp ->
                    update (FocusTab tp) newmodel


handleSubmit : List String -> FormData -> Model -> ( Model, Cmd Msg )
handleSubmit ident fd model =
    let
        strIdent =
            String.join "::" ident
    in
    case ident of
        "new-project" :: _ ->
            handleProjectSubmit strIdent fd model

        "import-options" :: _ ->
            handleImportSubmit fd model

        "global-settings" :: _ ->
            handleGlobalSubmit fd model

        "edit-project" :: _ ->
            handleProjectSubmit strIdent fd model

        _ ->
            -- This shouldn't be possible
            ( model, Cmd.none )


handleProjectSubmit : String -> FormData -> Model -> ( Model, Cmd Msg )
handleProjectSubmit ident fd model =
    case FParse.parseValidate FParse.json fd.fields of
        ( _, Ok jsonValue ) ->
            let
                forms =
                    Dict.remove ident model.forms

                ( vistas, command ) =
                    if ident == "new-project" then
                        ( model.vistas, createProject jsonValue )

                    else
                        ( Dict.remove ident model.vistas
                        , updateProject jsonValue
                        )
            in
            ( { model | forms = forms, vistas = vistas }
            , command
            )

        ( formFields, Err _ ) ->
            let
                pif =
                    { fd | submitted = False }

                forms =
                    Dict.insert ident pif model.forms
            in
            ( { model | forms = forms }, Cmd.none )


handleImportSubmit : FormData -> Model -> ( Model, Cmd Msg )
handleImportSubmit fd model =
    case FParse.parseValidate FParse.json fd.fields of
        ( _, Ok jsonValue ) ->
            let
                ipf =
                    importOptionsFields model.gconfig Nothing

                ipff =
                    { fd | fields = ipf }

                forms =
                    Dict.insert "import-options" ipff model.forms
            in
            ( { model | forms = forms }, importFile jsonValue )

        ( _, Err _ ) ->
            let
                ipff =
                    { fd | submitted = False }

                forms =
                    Dict.insert "import-options" ipff model.forms
            in
            ( { model | forms = forms }, Cmd.none )


handleGlobalSubmit : FormData -> Model -> ( Model, Cmd Msg )
handleGlobalSubmit fd model =
    case FParse.parseValidate FParse.json fd.fields of
        ( _, Ok jsonValue ) ->
            let
                gs =
                    GlobalSettings "" ""

                gsf =
                    { fd | fields = globalSettingsFields gs }

                forms =
                    Dict.insert "global-settings" gsf model.forms
            in
            ( { model | forms = forms }, updateGlobalSettings jsonValue )

        ( _, Err _ ) ->
            let
                gsf =
                    { fd | submitted = False }

                forms =
                    Dict.insert "global-settings" gsf model.forms
            in
            ( { model | forms = forms }, Cmd.none )


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
subscriptions model =
    Sub.batch
        [ receivedGlobalConfig ReceivedGlobalConfig
        , receivedProjectIndex ReceivedProjectIndex
        , receivedInterlinearIndex ReceivedInterlinearIndex
        , receivedDoc ReceivedDoc
        , newProject NewProjectMenu
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
roleAttr : String -> Html.Attribute msg
roleAttr role =
    Attr.attribute "role" role


viewUnknownProgress : List (Html.Attribute Msg) -> List (Html.Html Msg) -> Html.Html Msg
viewUnknownProgress attrs children =
    Html.node "progress" attrs children


view : Model -> Html.Html Msg
view model =
    let
        tabtree =
            treeifyTabs <| Dict.keys model.ventanas
    in
    Html.main_ [ Attr.class "grid-with-side" ]
        [ Html.aside [ Attr.class "side" ]
            [ Html.nav []
                [ Html.h2 [] [ Html.text "Projects" ]
                , viewProjects model
                ]
            ]
        , if Dict.isEmpty tabtree then
            Html.div
                [ Attr.class "container-fluid" ]
                [ if Set.isEmpty model.loading then
                    Html.h1 []
                        [ Html.text "Welcome to Your Menkayonta" ]

                  else
                    viewUnknownProgress [] []
                ]

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
    let
        ventana =
            Dict.get tp model.ventanas
                |> Maybe.withDefault
                    { title = "Error"
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
            , ( "tabnav", True )
            ]
        ]
        [ Html.text ventana.title ]


viewTab : Model -> TabPath -> Html.Html Msg
viewTab model tp =
    Html.div
        [ Attr.classList
            [ ( "focused", Just tp == model.focused )
            , ( "hidden", not (visMember tp model.visVentanas) )
            , ( "tabview", True )
            ]
        , Attr.style "max-height" (String.fromInt (model.windowHeight - 50) ++ "px")
        , Attr.style "height" (String.fromInt (model.windowHeight - 50) ++ "px")
        , Attr.height model.windowHeight
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
        [ Html.a
            [ Attr.href "#"
            , Event.onClick <| RequestProjectIndex p.identifier
            ]
            [ viewLoadingProject model p ]
        , Html.ul []
            [ Html.li []
                [ Html.a
                    [ Attr.href "#"
                    , Event.onClick <| ProjectSettingsEdit p
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
            ]
        ]


viewVista : Model -> TabPath -> Vista -> Html Msg
viewVista model tp vista =
    case vista.content of
        ProjectInfoContent pi ->
            case Dict.get vista.identifier model.forms of
                Just f ->
                    Html.form [ Event.onSubmit (FormSubmit tp) ]
                        [ Field.toHtml (ProjectInfoFormChange tp) f.fields
                        , Html.button [ Event.onClick (FormSubmit tp) ]
                            [ Html.text "Save" ]
                        , Html.button [ Event.onClick CloseTab ]
                            [ Html.text "Cancel" ]
                        ]

                Nothing ->
                    Html.text "No such form."

        ImportOptionsContent io ->
            case Dict.get "import-options" model.forms of
                Just f ->
                    Html.form [ Event.onSubmit (FormSubmit tp) ]
                        [ Field.toHtml ImportOptionsFormChange f.fields
                        , Html.button [ Event.onClick (FormSubmit tp) ]
                            [ Html.text "Save" ]
                        , Html.button [ Event.onClick CloseTab ]
                            [ Html.text "Cancel" ]
                        ]

                Nothing ->
                    Html.text "No such form."

        GlobalSettingsContent gs ->
            case Dict.get "global-settings" model.forms of
                Just f ->
                    Html.div []
                        [ Html.p []
                            [ Html.text
                                "Name and email address required."
                            ]
                        , Html.form [ Event.onSubmit (FormSubmit tp) ]
                            [ Field.toHtml GlobalSettingsFormChange f.fields
                            , Html.button [ Event.onClick (FormSubmit tp) ]
                                [ Html.text "Save" ]
                            , Html.button [ Event.onClick CloseTab ]
                                [ Html.text "Cancel" ]
                            ]
                        ]

                Nothing ->
                    Html.text "No such form."

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
                        [ Attr.type_ "text"
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
                                || String.contains ss i.annotations.glosses
                                || List.any
                                    (\t -> String.contains ss t.translation)
                                    (Dict.values i.translations)
                        )
                        ints

                is =
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
                    (List.map (viewInterlinearItem vista.project) is)
                ]

        InterlinearContent i ->
            let
                params =
                    Dict.get tp model.ventanas
                        |> Maybe.map .params
                        |> Maybe.withDefault defVParams
            in
            Html.div []
                [ Html.a
                    [ Attr.href "#"
                    , Event.onClick (ChangeEditParam tp)
                    ]
                    [ Html.text
                        (if params.edit then
                            "Revert to View"

                         else
                            "Edit"
                        )
                    ]
                , if params.edit then
                    --viewOldInterlinear vista.project i
                    Html.text "fix me"

                  else
                    --viewOldInterlinearForm vista.project i
                    Html.text "fix me"
                ]

        ErrorContent err ->
            viewError model err


viewError : Model -> Error -> Html Msg
viewError _ err =
    Html.text err.message


viewInterlinearItem : String -> M.Interlinear -> Html.Html Msg
viewInterlinearItem proj int =
    let
        srcLine =
            if int.annotations.breaks /= "" then
                viewGlosses int.text int.annotations.breaks int.annotations.glosses

            else
                Html.p [] [ Html.text int.text ]

        transLines =
            List.map (\t -> Html.p [] [ Html.text t.translation ]) (Dict.values int.translations)
    in
    Html.li []
        [ Html.div [] (srcLine :: transLines)
        , Html.a
            [ Attr.href "#"
            , Event.onClick (RequestDocId proj (UUID.toString int.id))
            ]
            [ Html.text "Open" ]
        ]


viewGlosses : String -> String -> String -> Html.Html Msg
viewGlosses src brk gls =
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
viewTranslation model dc =
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


listToTp : List Int -> Maybe TabPath
listToTp is =
    case is of
        c :: r :: t :: [] ->
            Just (tabpath c r t)

        _ ->
            Nothing


sToTp : String -> Maybe TabPath
sToTp s =
    String.split "," s
        |> List.map String.toInt
        |> ME.values
        |> listToTp


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

        "all-interlinears" ->
            D.map InterlinearsContent
                (D.field "content" (D.list M.interlinearDecoder))

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
    D.map4 ProjectInfo
        (D.field "title" D.string)
        (D.field "identifier" D.string)
        (D.field "enabled" D.bool)
        (D.field "url" (D.nullable D.string))


translationsDecoder : D.Decoder (List Translation)
translationsDecoder =
    D.list translationDecoder


translationDecoder : D.Decoder Translation
translationDecoder =
    D.map3 Translation
        (D.field "key" D.string)
        (D.field "value" D.string)
        (D.field "id" D.string)


projectParser : FParse.Parser FieldKind ProjectInfo
projectParser =
    FParse.map4 ProjectInfo
        (FParse.field ProjectIdentifier FParse.string)
        (FParse.field ProjectTitle FParse.string)
        (FParse.field ProjectEnabled FParse.bool)
        (FParse.field ProjectUrl (FParse.maybe FParse.string))


importParser : FParse.Parser FieldKind ImportOptions
importParser =
    FParse.map4 ImportOptions
        (FParse.field ImportFile (FParse.maybe FParse.string))
        (FParse.field ImportContent (FParse.maybe FParse.string))
        (FParse.field ImportKind FParse.string)
        (FParse.field ImportProject FParse.string)


globalParser : FParse.Parser FieldKind GlobalSettings
globalParser =
    FParse.map2 GlobalSettings
        (FParse.field GlobalName FParse.string)
        (FParse.field GlobalEmail FParse.email)


getByVista : String -> Dict TabPath Ventana -> Maybe TabPath
getByVista vista ventanas =
    List.head <| getAllByVista vista ventanas


getAllByVista : String -> Dict TabPath Ventana -> List TabPath
getAllByVista vista ventanas =
    List.filter (\( k, v ) -> v.vista == vista) (Dict.toList ventanas)
        |> List.map Tuple.first


getProjectTitle : String -> Model -> Maybe String
getProjectTitle projid model =
    model.gconfig
        |> Maybe.map .projects
        |> Maybe.andThen (LE.find (\x -> x.identifier == projid))
        |> Maybe.map .title
