module Display.Composite exposing (Params, view)

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
        , DocId(..)
        , Identifier(..)
        , Interlinear
        , Value(..)
        , identifierToString
        )


type alias Params msg =
    { composite : Composite
    , editEvent : Interlinear -> msg
    , listingEvent : Maybe String -> msg
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
                , viewInterlinear params int
                ]

        _ ->
            Html.div [] [ Html.text "doc not supported" ]


viewInterlinear : Params msg -> Interlinear -> Html.Html msg
viewInterlinear params int =
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
            [ if not (List.isEmpty params.composite.tags) then
                let
                    metaParams : Display.Meta.Params msg
                    metaParams =
                        { listingEvent = params.listingEvent }
                in
                Html.article []
                    [ Html.header []
                        [ Html.h3 []
                            [ Html.text "Tags" ]
                        ]
                    , tags metaParams params.composite.tags
                    ]

              else
                Html.text ""
            , if not (List.isEmpty params.composite.properties) then
                Html.article []
                    [ Html.header []
                        [ Html.h3 []
                            [ Html.text "Properties" ]
                        ]
                    , properties params.composite.properties
                    ]

              else
                Html.text ""
            , if not (List.isEmpty params.composite.descriptions) then
                Html.article []
                    [ Html.header []
                        [ Html.h3 []
                            [ Html.text "Descriptions" ]
                        ]
                    , descriptions params.composite.descriptions
                    ]

              else
                Html.text ""
            , Html.article [ Attr.id "modification-view" ]
                [ Html.header []
                    [ Html.h3 []
                        [ Html.text "Modifications" ]
                    ]
                , modifications params.composite.modifications
                ]
            ]
        ]
