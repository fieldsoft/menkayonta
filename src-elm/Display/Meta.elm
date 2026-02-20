module Display.Meta exposing
    ( description
    , descriptions
    , modification
    , modifications
    , properties
    , property
    , tag
    , tags
    )

import Html
import Html.Attributes as Attr
import Html.Events as Event
import Iso8601
import Menkayonta exposing (Description, Modification, Property, Tag)
import Time


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


tags : List Tag -> Html.Html msg
tags ts =
    Html.div [] <|
        List.map tag ts


tag : Tag -> Html.Html msg
tag t =
    Html.span [ Attr.class "tag" ] [ Html.text t.id.kind ]


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
