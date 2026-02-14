module Form.Project exposing
    ( Model
    , Msg(..)
    , fromProjectInfo
    , init
    , initData
    , update
    , view
    )

import Config exposing (ProjectInfo)
import Form.Shared
    exposing
        ( StringField
        , blankString
        , displayField
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
    , identifier : UUID.UUID
    , title : StringField
    , url : StringField
    , key : StringField
    }


type Msg
    = Title String
    | Url String
    | Save
    | Cancel
    | None
    | Key String


initData : UUID.UUID -> Model
initData uuid =
    { changed = False
    , submitted = False
    , error = "Please fill the empty form."
    , valid = False
    , identifier = uuid
    , title = { blankString | valid = False, error = "Cannot be empty." }
    , url = blankString
    , key = { blankString | valid = False, error = "Cannot be empty." }
    }


fromProjectInfo : ProjectInfo -> Model
fromProjectInfo pi =
    { changed = False
    , submitted = False
    , error = ""
    , valid = True
    , identifier = pi.identifier
    , title =
        { blankString
            | value = pi.title
            , original = pi.title
        }
    , url =
        { blankString
            | value = Maybe.withDefault "" pi.url
            , original = Maybe.withDefault "" pi.url
        }
    , key =
        { blankString
            | value = pi.key
            , original = pi.key
        }
    }


init : Model -> ( Model, Cmd Msg )
init model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        toperr : String
        toperr =
            "Please correct form."

        valid : Model -> Bool
        valid model_ =
            List.all identity
                [ model_.title.valid
                , model_.url.valid
                , model_.key.valid
                ]

        validate : Model -> Model
        validate model_ =
            { model_
                | changed = True
                , valid = valid model_
                , error =
                    if valid model_ then
                        ""

                    else
                        toperr
            }
    in
    case msg of
        -- A non-operation
        None ->
            ( model, Cmd.none )

        Title str ->
            let
                title : StringField
                title =
                    model.title
            in
            if String.isEmpty str then
                ( validate
                    { model
                        | title =
                            { title
                                | value = str
                                , valid = False
                                , error = "A title is required."
                                , changed = True
                            }
                    }
                , Cmd.none
                )

            else
                ( validate
                    { model
                        | title =
                            { title
                                | value = str
                                , valid = True
                                , error = ""
                                , changed = True
                            }
                    }
                , Cmd.none
                )

        Key str ->
            let
                key : StringField
                key =
                    model.url
            in
            if String.length str /= 1 then
                ( validate
                    { model
                        | key =
                            { key
                                | value = str
                                , valid = False
                                , error = "Keys are a single character."
                                , changed = True
                            }
                    }
                , Cmd.none
                )

            else
                ( validate
                    { model
                        | key =
                            { key
                                | value = str
                                , valid = True
                                , error = ""
                                , changed = True
                            }
                    }
                , Cmd.none
                )

        Url str ->
            let
                url : StringField
                url =
                    model.url
            in
            case ( String.isEmpty str, Url.fromString str ) of
                ( False, Nothing ) ->
                    let
                        err : String
                        err =
                            String.join " "
                                [ "A URL is not required,"
                                , "but it must be valid if provided."
                                ]
                    in
                    ( validate
                        { model
                            | url =
                                { url
                                    | value = str
                                    , valid = False
                                    , error = err
                                    , changed = True
                                }
                        }
                    , Cmd.none
                    )

                _ ->
                    ( validate
                        { model
                            | url =
                                { url
                                    | value = str
                                    , valid = True
                                    , error = ""
                                    , changed = True
                                }
                        }
                    , Cmd.none
                    )

        Save ->
            if validate model |> .valid then
                ( { model | submitted = True }, Cmd.none )

            else
                ( model, Cmd.none )

        Cancel ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.form []
        [ displayField
            { formname = "projectconf"
            , label = "Identifier"
            , kind = Html.input
            , oninput = \_ -> None
            , name = "identifier"
            , value = UUID.toString model.identifier
            , original = UUID.toString model.identifier
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
        , displayField
            { formname = "projectconf"
            , label = "Project Title"
            , kind = Html.input
            , oninput = Title
            , name = "title"
            , value = model.title.value
            , original = model.title.original
            , changed = model.title.changed
            , valid = model.title.valid
            , help = "A short title for the project."
            , error = model.title.error
            , disabled = False
            , deleted = False
            , spellcheck = True
            , options = []
            , id = Nothing
            }
        , displayField
            { formname = "projectconf"
            , label = "Project Key"
            , kind = Html.input
            , oninput = Key
            , name = "key"
            , value = model.key.value
            , original = model.key.original
            , changed = model.key.changed
            , valid = model.key.valid
            , help = "A one character key for visual reference, such as '†', 'A', or '🍁'."
            , error = model.key.error
            , disabled = False
            , deleted = False
            , spellcheck = False
            , options = []
            , id = Nothing
            }
        , displayField
            { formname = "projectcof"
            , label = "Server Url"
            , kind = Html.input
            , oninput = Url
            , name = "url"
            , value = model.url.value
            , original = model.url.original
            , changed = model.url.changed
            , valid = model.url.valid
            , help = "A URL to a server to work with others."
            , error = model.url.error
            , disabled = False
            , deleted = False
            , spellcheck = False
            , options = []
            , id = Nothing
            }
        , Html.button
            (if model.valid then
                [ Event.onClick Save
                , Attr.type_ "button"
                ]

             else
                [ Attr.attribute "data-tooltip" model.error
                , Attr.attribute "data-placement" "right"
                , Attr.type_ "button"
                , Event.onClick None
                ]
            )
            [ Html.text "Save" ]
        , Html.button
            [ Attr.class "secondary"
            , Attr.type_ "button"
            , Event.onClick Cancel
            ]
            [ Html.text "Cancel" ]
        ]
