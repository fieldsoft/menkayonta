module Form.Shared exposing
    ( FieldDescription
    , SelectField
    , StringField
    , blankSelect
    , blankString
    , displayField
    , displaySelectField
    )

import Html
import Html.Attributes as Attr
import Html.Events as Event


type alias StringField =
    { value : String
    , valid : Bool
    , error : String
    , changed : Bool
    , original : String
    }


type alias SelectField =
    { value : String
    , options : List ( String, String )
    , valid : Bool
    , error : String
    , changed : Bool
    , original : String
    }


blankString : StringField
blankString =
    { value = ""
    , valid = True
    , error = ""
    , changed = False
    , original = ""
    }


blankSelect : SelectField
blankSelect =
    { value = ""
    , options = []
    , valid = True
    , error = ""
    , changed = False
    , original = ""
    }


type alias FieldDescription msg =
    { formname : String
    , label : String
    , kind :
        List (Html.Attribute msg)
        -> List (Html.Html msg)
        -> Html.Html msg
    , oninput : String -> msg
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


displayField : FieldDescription msg -> Html.Html msg
displayField fd =
    let
        id : Int
        id =
            Maybe.withDefault -1 fd.id

        name : String
        name =
            [ fd.formname
            , fd.name
            ]
                |> String.join "-"

        helper : String
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


isInValidAttr : Bool -> Html.Attribute msg
isInValidAttr valid =
    Attr.attribute "aria-invalid"
        (if valid then
            "false"

         else
            "true"
        )


displaySelectField : FieldDescription msg -> Html.Html msg
displaySelectField fd =
    let
        id : Int
        id =
            Maybe.withDefault -1 fd.id

        name : String
        name =
            [ fd.formname
            , fd.name
            ]
                |> String.join "-"

        helper : String
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
