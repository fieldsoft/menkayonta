module Display.Meta exposing
    ( Msg
    , descriptions
    , links
    , modifications
    , properties
    , tagField
    , tags
    )

import DateFormat as DF
import Html
import Html.Attributes as Attr
import Html.Events as Event
import Menkayonta
    exposing
        ( Description
        , Identifier(..)
        , Link
        , Modification
        , Property
        , Tag
        , identifierToReverse
        , identifierToString
        )
import Meta exposing (TagField)
import Msg exposing (RequestType(..))
import Time
import UUID
import Url


type alias Msg =
    Msg.Msg


openval : String
openval =
    "!!!OPEN!!!"


properties : List Property -> Html.Html Msg
properties props =
    Html.table [ Attr.class "striped" ]
        [ Html.thead []
            [ Html.tr []
                [ Html.th [ Attr.attribute "scope" "col" ]
                    [ Html.text "Attribute" ]
                , Html.th [ Attr.attribute "scope" "col" ]
                    [ Html.text "Value" ]
                ]
            ]
        , Html.tbody [] <| List.map property props
        ]


property : Property -> Html.Html Msg
property prop =
    Html.tr []
        [ Html.td []
            [ Html.text prop.id.kind ]
        , Html.td []
            [ Html.text prop.id.value ]
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


tags : UUID.UUID -> List Tag -> Html.Html Msg
tags project ts =
    case ts of
        [] ->
            Html.text ""

        t :: _ ->
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
                                        { value = openval
                                        , docids = [ t.id.docid ]
                                        , project = project
                                        }
                            ]
                            [ Html.text "+" ]
                        ]
                    ]
                , Html.div [] <|
                    List.map (tag project) ts
                ]


tag : UUID.UUID -> Tag -> Html.Html Msg
tag project t =
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
            ]
            [ Html.text "×" ]
        ]


tagField : TagField -> Html.Html Msg
tagField tagfield =
    let
        isValid : Bool
        isValid =
            not (String.isEmpty tagfield.value)
                || tagfield.value
                == openval
    in
    Html.fieldset []
        [ Html.label []
            [ Html.text "New Tag"
            , Html.input
                [ Attr.value
                    (if tagfield.value == openval then
                        ""

                     else
                        tagfield.value
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
                            (Just { tagfield | value = val })
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


descriptions : List Description -> Html.Html Msg
descriptions descs =
    Html.div [] <|
        List.map description descs


description : Description -> Html.Html Msg
description desc =
    Html.details []
        [ Html.summary []
            [ Html.text desc.id.kind ]
        , Html.p []
            [ Html.text desc.value ]
        ]
