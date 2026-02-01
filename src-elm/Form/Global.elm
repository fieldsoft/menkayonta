module Form.Global exposing
    ( Data
    , Field
    , change
    , display
--    , init
    , initData
    )

import Form.Shared
    exposing
        ( FieldDescription
        , SelectField
        , StringField
        , blankSelect
        , blankString
        , displayField
        )
import Html
import Html.Attributes as Attr
import Html.Events as Event
import Email


{-| The Data type alias describes a form representation of non-project
global configuration for the app.
-}
type alias Data =
    { changed : Bool
    , submitted : Bool
    , error : String
    , valid : Bool
    , name : StringField
    , email : StringField
    }


{-| These are the fields of the form. Buttons are considered a field.
-}
type Field
    = Email
    | Name
    | Save
    | Cancel
    | None


{-| Alias for callbacks sent when display is called. -}
type alias Callbacks msg =
    { close : Field -> String -> msg
    , change : Field -> String -> msg
    }


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


{-| This handles events specific to particular fields.
-}
change : Field -> String -> Data -> Data
change fid str d =
    let
        email =
            d.email

        name =
            d.name

        toperr =
            "Please correct form."

        valid d_ =
            List.all identity
                [ d_.email.valid
                , d_.name.valid
                ]

        defD d_ =
            let
                valid_ =
                    valid d_
            in
            { d_
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
        -- A non-operation.
        None ->
            d
                
        Email ->
            if String.isEmpty str then
                { d
                    | email =
                        { email
                            | value = str
                            , valid = False
                            , error = "An email address is required."
                            , changed = True
                        }
                }
                    |> defD

            else
                case Email.fromString str of
                    Nothing ->
                        { d
                            | email =
                                { email
                                    | value = str
                                    , valid = False
                                    , error = "Invalid email address."
                                    , changed = True
                                }
                        }
                            |> defD

                    Just _ ->
                        { d
                            | email =
                                { email
                                    | value = str
                                    , valid = True
                                    , error = ""
                                    , changed = True
                                }
                        }
                            |> defD

        Name ->
            if String.isEmpty str then
                { d
                    | name =
                        { name
                            | value = str
                            , valid = False
                            , error = "A name is required."
                            , changed = True
                        }
                }
                    |> defD

            else
                { d
                    | name =
                        { name
                            | value = str
                            , valid = True
                            , error = ""
                            , changed = True
                        }
                }
                    |> defD

        Save ->
            if defD d |> .valid then
                { d | submitted = True }

            else
                d

        Cancel ->
            initData


{-| Display the form. The function f likely wraps types that are not
relevant to the form logic but that result in changes that are handled
in `change`
-}
display : Data -> Callbacks  msg -> Html.Html msg
display d c =
    Html.form []
        [ displayField
            { formname = "globalsettings"
            , label = "Email Address"
            , kind = Html.input
            , oninput = c.change Email
            , name = "email"
            , value = d.email.value
            , original = d.email.original
            , changed = d.email.changed
            , valid = d.email.valid
            , help = "Your email address."
            , error = d.email.error
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
            , oninput = c.change Name
            , name = "name"
            , value = d.name.value
            , original = d.name.original
            , changed = d.name.changed
            , valid = d.name.valid
            , help = "Your name."
            , error = d.name.error
            , disabled = False
            , deleted = False
            , spellcheck = False
            , options = []
            , id = Nothing
            }
        , Html.button
            (if d.valid then
                [ Event.onClick <| c.change Save ""
                , Attr.type_ "button"
                ]

             else
                [ Attr.attribute "data-tooltip" d.error
                , Attr.attribute "data-placement" "right"
                , Attr.type_ "button"
                , Event.onClick <| c.change None ""
                ]
            )
            [ Html.text "Save" ]
        , Html.button
            [ Attr.class "secondary"
            , Attr.type_ "button"
            , Event.onClick <|
                c.close Cancel ""
            ]
            [ Html.text "Cancel" ]
        ]
