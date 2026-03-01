module Display.Meta exposing
    ( Msg
    , descriptions
    , links
    , modifications
    , properties
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
import Msg exposing (RequestType(..))
import Time
import UUID
import Url


type alias Msg =
    Msg.Msg
      

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
    Html.div [] <|
        List.map (tag project) ts


tag : UUID.UUID -> Tag -> Html.Html Msg
tag project t =
    Html.a
        [ Attr.href "#"
        , Attr.class "tag"
        , t.id
            |> MyTagId
            |> identifierToReverse
            |> OReversal
            |> Msg.Request project
            |> Msg.UserClick
            |> Event.onClick
        ]
        [ Html.text t.id.kind ]


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
