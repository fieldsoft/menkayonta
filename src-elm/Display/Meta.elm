module Display.Meta exposing
    ( Params
    , descriptions
    , modifications
    , properties
    , tags
    )

import Html
import Html.Attributes as Attr
import Html.Events as Event
import Iso8601
import Menkayonta
    exposing
        ( Description
        , Identifier(..)
        , Modification
        , Property
        , Tag
        , identifierToReverse
        )
import Time


type alias Params msg =
    { listingEvent : Maybe String -> msg }


properties : List Property -> Html.Html msg
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


property : Property -> Html.Html msg
property prop =
    Html.tr []
        [ Html.td []
            [ Html.text prop.id.kind ]
        , Html.td []
            [ Html.text prop.id.value ]
        ]


modifications : List Modification -> Html.Html msg
modifications mods =
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
            List.map modification <|
                List.reverse <|
                    List.sortBy (\m -> Time.posixToMillis m.id.time) mods
        ]


modification : Modification -> Html.Html msg
modification mod =
    Html.tr []
        [ Html.td []
            [ Html.text mod.id.kind ]
        , Html.td []
            [ Html.text <| Iso8601.fromTime mod.id.time ]
        ]


tags : Params msg -> List Tag -> Html.Html msg
tags params ts =
    Html.div [] <|
        List.map (tag params) ts


tag : Params msg -> Tag -> Html.Html msg
tag params t =
    Html.a
        [ Attr.href "#"
        , Attr.class "tag"
        , t.id
            |> MyTagId
            |> identifierToReverse
            |> params.listingEvent
            |> Event.onClick
        ]
        [ Html.text t.id.kind ]


descriptions : List Description -> Html.Html msg
descriptions descs =
    Html.div [] <|
        List.map description descs


description : Description -> Html.Html msg
description desc =
    Html.details []
        [ Html.summary []
            [ Html.text desc.id.kind ]
        , Html.p []
            [ Html.text desc.value ]
        ]
