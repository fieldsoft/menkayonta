module Form.Importer exposing
    ( Model
    , Msg(..)
    , init
    , initData
    , update
    , view
    )

import Form.Shared
    exposing
        ( FieldDescription
        , SelectField
        , blankSelect
        , displayField
        , displaySelectField
        )
import Html
import Html.Attributes as Attr
import Html.Events as Event
import UUID
import Url


type alias Model =
    { changed : Bool
    , submitted : Bool
    , error : String
    , valid : Bool
    , filepath : String
    , kind : SelectField
    , project : SelectField
    }


type Msg
    = Kind String
    | None
    | Project String
    | Import
    | Cancel


initData : Model
initData =
    { changed = False
    , submitted = False
    , error = "Please fill the empty form."
    , valid = False
    , filepath = ""
    , kind =
        { blankSelect
            | options =
                [ ( "Dative Form Json"
                  , "Dative Form Json"
                  )
                ]
        }
    , project = blankSelect
    }


init : String -> List ( String, String ) -> ( Model, Cmd Msg )
init filepath projectOptions =
    let
        model : Model
        model =
            { initData
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
                    { blankSelect | options = projectOptions }
            }
    in
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        kind : SelectField
        kind =
            model.kind

        kindopts : List String
        kindopts =
            List.map Tuple.second kind.options

        project : SelectField
        project =
            model.project

        projopts : List String
        projopts =
            List.map Tuple.second project.options

        toperr : String
        toperr =
            "Pleae correct form."

        valid : Model -> Bool
        valid model_ =
            List.all identity
                [ model_.kind.valid
                , model_.project.valid
                ]

        validate : Model -> Model
        validate model_ =
            let
                valid_ : Bool
                valid_ =
                    valid model_
            in
            { model_
                | changed = True
                , valid = valid_
                , error =
                    if valid_ then
                        ""

                    else
                        toperr
            }
    in
    case msg of
        None ->
            ( model, Cmd.none )

        Kind str ->
            if List.member str kindopts then
                ( validate
                    { model
                        | kind =
                            { kind
                                | value = str
                                , valid = True
                                , changed = True
                                , error = ""
                            }
                    }
                , Cmd.none
                )

            else
                ( validate
                    { model
                        | kind =
                            { kind
                                | value = ""
                                , valid = False
                                , changed = True
                                , error = "Choose an import type."
                            }
                    }
                , Cmd.none
                )

        Project str ->
            if List.member str projopts then
                ( validate
                    { model
                        | project =
                            { project
                                | value = str
                                , valid = True
                                , changed = True
                                , error = ""
                            }
                    }
                , Cmd.none
                )

            else
                ( validate
                    { model
                        | project =
                            { project
                                | value = ""
                                , valid = False
                                , changed = True
                                , error = "Choose a project."
                            }
                    }
                , Cmd.none
                )

        Import ->
            if validate model |> .valid then
                ( { model | submitted = True }, Cmd.none )

            else
                ( model, Cmd.none )

        Cancel ->
            ( initData, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.form []
        [ Html.fieldset []
            [ displayField
                { formname = "importoptions"
                , label = "File Path"
                , kind = Html.input
                , oninput = \_ -> None
                , name = "filepath"
                , value = model.filepath
                , original = model.filepath
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
            , displaySelectField
                { formname = "importoptions"
                , label = "Import Type"
                , kind = Html.select
                , oninput = Kind
                , name = "kind"
                , value = model.kind.value
                , original = model.kind.original
                , changed = model.kind.changed
                , valid = model.kind.valid
                , help = "The format of the import file."
                , error = model.kind.error
                , disabled = False
                , deleted = False
                , spellcheck = False
                , options = model.kind.options
                , id = Nothing
                }
            , displaySelectField
                { formname = "importoptions"
                , label = "Project"
                , kind = Html.select
                , oninput = Project
                , name = "project"
                , value = model.project.value
                , original = model.project.original
                , changed = model.project.changed
                , valid = model.project.valid
                , help = "The project that receives the import."
                , error = model.project.error
                , disabled = False
                , deleted = False
                , spellcheck = False
                , options = model.project.options
                , id = Nothing
                }
            ]
        , Html.button
            (if model.valid then
                [ Event.onClick Import
                , Attr.type_ "button"
                ]

             else
                [ Attr.attribute "data-tooltip" model.error
                , Attr.attribute "data-placement" "right"
                , Attr.type_ "button"
                , Event.onClick None
                ]
            )
            [ Html.text "Import" ]
        , Html.button
            [ Attr.class "secondary"
            , Attr.type_ "button"
            , Event.onClick Cancel
            ]
            [ Html.text "Cancel" ]
        ]
