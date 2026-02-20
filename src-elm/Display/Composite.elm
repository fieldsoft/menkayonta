module Display.Composite exposing (view, Params)

import Display.InterlinearListing
import Display.Meta
    exposing
        ( descriptions
        , modifications
        , properties
        , tags
        )
import Html
import Html.Attributes as Attr
import Html.Events as Event
import Menkayonta
    exposing
        ( Composite
        , Interlinear
        , Value(..)
        , Translation
        , identifierToString
        , Identifier(..)
        , DocId(..)
        )
import UUID


type alias Params msg =
    { composite : Composite
    , editEvent : Interlinear -> msg
    }


view : Params msg -> Html.Html msg
view params =
    case params.composite.doc of
        Just (MyInterlinear int) ->
            Html.div []
                [ Html.nav []
                    [ Html.ul []
                        [ Html.li []
                            [ Html.a
                                [ Attr.href "#"
                                , Event.onClick <|
                                    params.editEvent int
                                ]
                                [ Html.text "Edit" ]
                            ]
                        ]
                    ]
                , viewInterlinear params.composite int
                ]

        _ ->
            Html.div [] [ Html.text "doc not supported" ]


viewInterlinear : Composite -> Interlinear -> Html.Html msg
viewInterlinear comp int =
    Html.div [ Attr.class "docview" ]
        [ Html.h2 []
            [ Html.text "Interlinear Gloss" ]
        , Html.article []
            [ Display.InterlinearListing.viewInterlinear int
            , Html.footer []
                [ InterlinearId int.id
                    |> MyDocId
                    |> identifierToString
                    |> (\x ->
                            "ID: "
                                ++ x
                                |> Html.text
                       )
                ]
            ]
        , Html.h2 []
            [ Html.text "Metadata" ]
        , Html.div [ Attr.class "metaview" ]
            [ if not (List.isEmpty comp.tags) then
                Html.article []
                    [ Html.header []
                        [ Html.h3 []
                            [ Html.text "Tags" ]
                        ]
                    , tags comp.tags
                    ]

              else
                Html.text ""
            , if not (List.isEmpty comp.properties) then
                Html.article []
                    [ Html.header []
                        [ Html.h3 []
                            [ Html.text "Properties" ]
                        ]
                    , properties comp.properties
                    ]

              else
                Html.text ""
            , if not (List.isEmpty comp.descriptions) then
                Html.article []
                    [ Html.header []
                        [ Html.h3 []
                            [ Html.text "Descriptions" ]
                        ]
                    , descriptions comp.descriptions
                    ]

              else
                Html.text ""
            , Html.article [ Attr.id "modification-view" ]
                [ Html.header []
                    [ Html.h3 []
                        [ Html.text "Modifications" ]
                    ]
                , modifications comp.modifications
                ]
            ]
        ]
