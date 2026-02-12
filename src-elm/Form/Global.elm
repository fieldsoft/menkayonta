module Form.Global exposing
    ( Model
    , Msg(..)
    , update
    , view
    , init
    , initData
    )

import Config
    exposing
        ( GlobalConfig
        , GlobalSettings
        , ProjectInfo
        , globalConfigDecoder
        , projectInfoDecoder
        )
import Email
import Form.Shared
    exposing
        ( FieldDescription
        , StringField
        , blankString
        , displayField
        )
import Html
import Html.Attributes as Attr
import Html.Events as Event
import Json.Encode as E
import Json.Decode as D
import Task


{-| The Data type alias describes a form representation of non-project
global configuration for the app.
-}
type alias Model =
    { changed : Bool
    , submitted : Bool
    , error : String
    , valid : Bool
    , name : StringField
    , email : StringField
    }


{-| These are the fields of the form. Buttons are considered a field.
-}
type Msg
    = Email String
    | Name String
    | Save
    | Cancel
    | None


sendMsg : Msg -> Cmd Msg
sendMsg msg =
    Task.succeed msg
        |> Task.perform identity


{-| A starter Data instance.
-}
initData =
    { changed = False
    , submitted = False
    , error = "Please fill the empty form."
    , valid = False
    , email = { blankString | valid = False, error = "Cannot be empty." }
    , name = { blankString | valid = False, error = "Cannot be empty." }
    }


init : E.Value -> ( Model, Cmd Msg )
init value =
    case D.decodeValue globalConfigDecoder value of
        Err _ ->
            ( initData, Cmd.none )

        Ok gf ->
            ( initData
            , Cmd.batch
                  [ sendMsg <| Email gf.email
                  , sendMsg <| Name gf.name
                  ]
            )
        

{-| This handles events specific to particular fields.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        email =
            model.email

        name =
            model.name

        toperr =
            "Please correct form."

        valid model_ =
            List.all identity
                [ model_.email.valid
                , model_.name.valid
                ]

        validate model_ =
            let
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
        -- A non-operation.
        None ->
            ( model, Cmd.none )

        Email str ->
            if String.isEmpty str then
                ( validate
                      { model
                          | email =
                            { email
                                | value = str
                                , valid = False
                                , error = "An email address is required."
                                , changed = True
                            }
                      }
                , Cmd.none
                )

            else
                case Email.fromString str of
                    Nothing ->
                        ( validate
                              { model
                                  | email =
                                    { email
                                        | value = str
                                        , valid = False
                                        , error = "Invalid email address."
                                        , changed = True
                                    }
                              }
                        , Cmd.none
                        )

                    Just _ ->
                        ( validate
                              { model
                                  | email =
                                    { email
                                        | value = str
                                        , valid = True
                                        , error = ""
                                        , changed = True
                                    }
                              }
                        , Cmd.none
                        )

        Name str ->
            if String.isEmpty str then
                ( validate
                      { model
                          | name =
                            { name
                                | value = str
                                , valid = False
                                , error = "A name is required."
                                , changed = True
                            }
                      }
                , Cmd.none
                )

            else
                ( validate
                      { model
                          | name =
                            { name
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
            ( initData, Cmd.none )


{-| View the form.
-}
view : Model -> Html.Html Msg
view model =
    Html.form []
        [ displayField
            { formname = "globalsettings"
            , label = "Email Address"
            , kind = Html.input
            , oninput = Email
            , name = "email"
            , value = model.email.value
            , original = model.email.original
            , changed = model.email.changed
            , valid = model.email.valid
            , help = "Your email address."
            , error = model.email.error
            , disabled = False
            , deleted = False
            , spellcheck = False
            , options = []
            , id = Nothing
            }
        , displayField
            { formname = "globalsettings"
            , label = "Name"
            , kind = Html.input
            , oninput = Name
            , name = "name"
            , value = model.name.value
            , original = model.name.original
            , changed = model.name.changed
            , valid = model.name.valid
            , help = "Your name."
            , error = model.name.error
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
