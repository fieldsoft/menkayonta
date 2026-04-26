module Display.Meta exposing
    ( Msg
    , links
    , modifications
    , properties
    , propertyField
    , tagField
    , tags
    )

import DateFormat as DF
import Html
import Html.Attributes as Attr
import Html.Events as Event
import List.Extra as LE
import Menkayonta
    exposing
        ( DocId
        , Identifier(..)
        , Link
        , Modification
        , Property
        , Tag
        , docIdToDoctypeString
        , identifierToReverse
        , identifierToString
        )
import Meta exposing (PropertyField, TagField)
import Msg exposing (RequestType(..))
import Time
import UUID
import Url


type alias Msg =
    Msg.Msg


openval : String
openval =
    "!!!OPEN!!!"


properties : UUID.UUID -> DocId -> List Property -> Html.Html Msg
properties project docid props =
    case props of
        [] ->
            Html.article []
                [ Html.a
                    [ Attr.href "#"
                    , Attr.title "Add a new property"
                    , Attr.attribute "role" "button"
                    , Attr.class "secondary"
                    , Event.onClick <|
                        Msg.ChangeProperty <|
                            Just
                                { kind = openval
                                , value = openval
                                , docids = [ docid ]
                                , project = project
                                }
                    ]
                    [ Html.text "Add Property" ]
                ]

        _ ->
            Html.article []
                [ Html.header []
                    [ Html.h3 []
                        [ Html.text "Properties "
                        , Html.a
                            [ Attr.href "#"
                            , Attr.title "Add a new property"
                            , Attr.attribute "role" "button"
                            , Attr.class "secondary"
                            , Event.onClick <|
                                Msg.ChangeProperty <|
                                    Just
                                        { kind = openval
                                        , value = openval
                                        , docids = [ docid ]
                                        , project = project
                                        }
                            ]
                            [ Html.text "+" ]
                        ]
                    ]
                , Html.table [ Attr.class "striped" ]
                    [ Html.thead []
                        [ Html.tr []
                            [ Html.td
                                []
                                []
                            , Html.th
                                [ Attr.attribute "scope" "col" ]
                                [ Html.text "Attribute" ]
                            , Html.th
                                [ Attr.attribute "scope" "col" ]
                                [ Html.text "Value" ]
                            ]
                        ]
                    , Html.tbody [] <| List.map (property project docid) props
                    ]
                ]


property : UUID.UUID -> DocId -> Property -> Html.Html Msg
property project docid prop =
    let
        propstring : String
        propstring =
            String.join " : "
                [ prop.id.kind, prop.id.value ]

        attribute : String -> Property -> Html.Html Msg
        attribute str p =
            Html.a
                [ Attr.href "#"
                , Attr.title <| "View items with the property attribute: " ++ p.id.kind
                , p.id
                    |> MyPropertyId
                    |> identifierToReverse
                    |> Maybe.map (String.split "/")
                    |> Maybe.map (LE.setAt 3 "*")
                    |> Maybe.map (String.join "/")
                    |> OReversal
                    |> Msg.Request project
                    |> Msg.UserClick
                    |> Event.onClick
                ]
                [ Html.text str ]

        proppart : String -> Property -> Html.Html Msg
        proppart str p =
            Html.a
                [ Attr.href "#"
                , Attr.title <| "View items with the property: " ++ propstring
                , p.id
                    |> MyPropertyId
                    |> identifierToReverse
                    |> OReversal
                    |> Msg.Request project
                    |> Msg.UserClick
                    |> Event.onClick
                ]
                [ Html.text str ]
    in
    Html.tr []
        [ Html.td []
            [ Html.a
                [ Attr.href "#"
                , Attr.title <| "Revove the property: " ++ propstring
                , prop.id
                    |> MyPropertyId
                    |> identifierToString
                    |> ODelete prop.rev
                    |> Msg.Request project
                    |> Event.onClick
                ]
                [ Html.text "×" ]
            ]
        , Html.td []
            [ attribute prop.id.kind prop ]
        , Html.td []
            [ proppart prop.id.value prop ]
        ]


propertyField : PropertyField -> Html.Html Msg
propertyField propfield =
    let
        isValidK : Bool
        isValidK =
            not (String.isEmpty propfield.kind)
                || propfield.kind
                == openval

        isValidV : Bool
        isValidV =
            not (String.isEmpty propfield.value)
                || propfield.value
                == openval
    in
    Html.fieldset []
        [ Html.label []
            [ Html.text "New Attribute"
            , Html.input
                [ Attr.value
                    (if propfield.kind == openval then
                        ""

                     else
                        propfield.kind
                    )
                , Attr.type_ "text"
                , Attr.name "kindfield"
                , Attr.attribute "aria-label" "New Attribute"
                , Attr.attribute "aria-describedby" "kindfield-desc"
                , Attr.spellcheck True
                , if isValidK then
                    Attr.attribute "aria-invalid" "false"

                  else
                    Attr.attribute "aria-invalid" "true"
                , Event.onInput <|
                    \val ->
                        Msg.ChangeProperty
                            (Just { propfield | kind = val })
                ]
                []
            , Html.small
                [ Attr.id "kindfield-desc" ]
                [ if isValidK then
                    Html.text
                        (String.concat
                            [ "A short property attibute name. "
                            , "Avoid punctuation and spaces. "
                            , "Complex names aren't needed. "
                            , "One can always add additional metadata."
                            ]
                        )

                  else
                    Html.text "The attribute name can't be blank."
                ]
            ]
        , Html.label []
            [ Html.text "New Value"
            , Html.input
                [ Attr.value
                    (if propfield.value == openval then
                        ""

                     else
                        propfield.value
                    )
                , Attr.type_ "text"
                , Attr.name "valuefield"
                , Attr.attribute "aria-label" "New Value"
                , Attr.attribute "aria-describedby" "valuefield-desc"
                , Attr.spellcheck True
                , if isValidV then
                    Attr.attribute "aria-invalid" "false"

                  else
                    Attr.attribute "aria-invalid" "true"
                , Event.onInput <|
                    \val ->
                        Msg.ChangeProperty
                            (Just { propfield | value = val })
                ]
                []
            , Html.small
                [ Attr.id "valuefield-desc" ]
                [ if isValidV then
                    Html.text
                        (String.concat
                            [ "A short property value. "
                            , "Avoid punctuation and spaces. "
                            , "Don't try to stuff in multiple values. "
                            , "You can add multiple attribute-value "
                            , "pairs with the same attribute name."
                            ]
                        )

                  else
                    Html.text "The attribute name can't be blank."
                ]
            ]
        , Html.button
            (if isValidK && isValidV then
                [ Attr.type_ "button"
                , Event.onClick <|
                    Msg.SaveProperty propfield
                ]

             else
                [ Attr.attribute "data-tooltip" "Fields cannot be blank"
                , Attr.attribute "data-placement" "below"
                , Attr.type_ "button"
                , Event.onClick Msg.None
                ]
            )
            [ Html.text "Save" ]
        , Html.button
            [ Attr.class "secondary"
            , Attr.type_ "button"
            , Event.onClick <|
                Msg.ChangeProperty Nothing
            ]
            [ Html.text "Cancel" ]
        ]


links : List Link -> Html.Html Msg
links ls =
    Html.table [ Attr.class "striped" ]
        [ Html.thead []
            [ Html.tr []
                [ Html.th [ Attr.attribute "scope" "col" ]
                    [ Html.text "Relationship" ]
                , Html.th [ Attr.attribute "scope" "col" ]
                    [ Html.text "Value" ]
                ]
            ]
        , List.map link ls
            |> Html.tbody []
        ]


link : Link -> Html.Html Msg
link l =
    let
        idstring : String
        idstring =
            l.id.toid
                |> MyDocId
                |> identifierToString
    in
    Html.tr []
        [ Html.td []
            [ Html.text l.id.kind ]
        , Html.td []
            [ Html.text
                (idstring
                    |> Url.percentDecode
                    |> Maybe.withDefault idstring
                )
            ]
        ]


modifications : List Modification -> Html.Html Msg
modifications mods =
    Html.table [ Attr.class "striped" ]
        [ Html.thead []
            [ Html.tr []
                [ Html.th [ Attr.attribute "scope" "col" ]
                    [ Html.text "Event" ]
                , Html.th [ Attr.attribute "scope" "col" ]
                    [ Html.text "Time (UTC)" ]
                , Html.th [ Attr.attribute "scope" "col" ]
                    [ Html.text "Performed By" ]
                ]
            ]
        , Html.tbody [] <|
            List.map modification <|
                List.reverse <|
                    List.sortBy (\m -> Time.posixToMillis m.id.time) mods
        ]


modification : Modification -> Html.Html Msg
modification mod =
    let
        datetime : Time.Posix -> String
        datetime time =
            DF.format
                [ DF.hourMilitaryFixed
                , DF.text ":"
                , DF.minuteFixed
                , DF.text " "
                , DF.monthNameAbbreviated
                , DF.text " "
                , DF.dayOfMonthSuffix
                , DF.text ", "
                , DF.yearNumber
                ]
                Time.utc
                time

        person : String
        person =
            case mod.id.person of
                Menkayonta.PersonId str ->
                    str

                _ ->
                    "unknown"
    in
    Html.tr []
        [ Html.td []
            [ Html.text mod.id.kind ]
        , Html.td []
            [ Html.text <| datetime mod.id.time ]
        , Html.td []
            [ Html.text person ]
        ]


tags : UUID.UUID -> DocId -> List Tag -> Html.Html Msg
tags project docid ts =
    case ts of
        [] ->
            Html.article []
                [ Html.a
                    [ Attr.href "#"
                    , Attr.title "Add a new tag"
                    , Attr.attribute "role" "button"
                    , Attr.class "secondary"
                    , Event.onClick <|
                        Msg.ChangeTag <|
                            Just
                                { kind = openval
                                , docids = [ docid ]
                                , project = project
                                }
                    ]
                    [ Html.text "Add Tag" ]
                ]

        _ :: _ ->
            Html.article []
                [ Html.header []
                    [ Html.h3 []
                        [ Html.text "Tags "
                        , Html.a
                            [ Attr.href "#"
                            , Attr.title "Add a new tag"
                            , Attr.attribute "role" "button"
                            , Attr.class "secondary"
                            , Event.onClick <|
                                Msg.ChangeTag <|
                                    Just
                                        { kind = openval
                                        , docids = [ docid ]
                                        , project = project
                                        }
                            ]
                            [ Html.text "+" ]
                        ]
                    ]
                , Html.div [] <|
                    List.map (tag project docid) ts
                ]


tag : UUID.UUID -> DocId -> Tag -> Html.Html Msg
tag project docid t =
    Html.span
        [ Attr.class "tag" ]
        [ Html.a
            [ Attr.href "#"
            , Attr.title <| "View items tagged: " ++ t.id.kind
            , t.id
                |> MyTagId
                |> identifierToReverse
                |> OReversal
                |> Msg.Request project
                |> Msg.UserClick
                |> Event.onClick
            ]
            [ Html.text t.id.kind ]
        , Html.a
            [ Attr.href "#"
            , Attr.title <| "Revove the tag: " ++ t.id.kind
            , t.id
                |> MyTagId
                |> identifierToString
                |> ODelete t.rev
                |> Msg.Request project
                |> Event.onClick
            ]
            [ Html.text "×" ]
        ]


tagField : TagField -> Html.Html Msg
tagField tagfield =
    let
        isValid : Bool
        isValid =
            not (String.isEmpty tagfield.kind)
                || tagfield.kind
                == openval
    in
    Html.fieldset []
        [ Html.label []
            [ Html.text "New Tag"
            , Html.input
                [ Attr.value
                    (if tagfield.kind == openval then
                        ""

                     else
                        tagfield.kind
                    )
                , Attr.type_ "text"
                , Attr.name "tagfield"
                , Attr.attribute "aria-label" "New Tag"
                , Attr.attribute "aria-describedby" "tagfield-desc"
                , Attr.spellcheck True
                , if isValid then
                    Attr.attribute "aria-invalid" "false"

                  else
                    Attr.attribute "aria-invalid" "true"
                , Event.onInput <|
                    \val ->
                        Msg.ChangeTag
                            (Just { tagfield | kind = val })
                ]
                []
            , Html.small
                [ Attr.id "tagfield-desc" ]
                [ if isValid then
                    Html.text
                        (String.concat
                            [ "A short tag name. "
                            , "Avoid punctuation and spaces. "
                            , "Complex tag names aren't needed. "
                            , "One can always add additional tags or "
                            , "a different metadata type, "
                            , "such as a property."
                            ]
                        )

                  else
                    Html.text "The tag name can't be blank."
                ]
            ]
        , Html.button
            (if isValid then
                [ Attr.type_ "button"
                , Event.onClick <|
                    Msg.SaveTag tagfield
                ]

             else
                [ Attr.attribute "data-tooltip" "Cannot be blank"
                , Attr.attribute "data-placement" "below"
                , Attr.type_ "button"
                , Event.onClick Msg.None
                ]
            )
            [ Html.text "Save" ]
        , Html.button
            [ Attr.class "secondary"
            , Attr.type_ "button"
            , Event.onClick <|
                Msg.ChangeTag Nothing
            ]
            [ Html.text "Cancel" ]
        ]
