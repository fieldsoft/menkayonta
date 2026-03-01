module Display.Composite exposing (Model, Msg, view)

import Display.InterlinearListing
import Display.Meta
    exposing
        ( descriptions
        , links
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
import Msg
import UUID


type alias Msg =
      Msg.Msg
          

type alias Model =
    { composite : Composite
    , project : UUID.UUID
    }


view : Model -> Html.Html Msg
view model =
    case model.composite.doc of
        Just (MyInterlinear int) ->
            Html.div []
                [ Html.nav []
                    [ Html.ul []
                        [ Html.li []
                            [ Html.a
                                [ Attr.href "#"
                                , Msg.EditInterlinear model.project int
                                    |> Msg.UserClick
                                    |> Event.onClick
                                ]
                                [ Html.text "Edit" ]
                            ]
                        ]
                    ]
                , viewInterlinear model int
                ]

        _ ->
            Html.div [] [ Html.text "doc not supported" ]


viewInterlinear : Model -> Interlinear -> Html.Html Msg
viewInterlinear model int =
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
            [ if not (List.isEmpty model.composite.tags) then
                  tags model.project model.composite.tags
              else
                Html.text ""
            , if not (List.isEmpty model.composite.properties) then
                Html.article []
                    [ Html.header []
                        [ Html.h3 []
                            [ Html.text "Properties" ]
                        ]
                    , properties model.composite.properties
                    ]

              else
                Html.text ""
            , if not (List.isEmpty model.composite.descriptions) then
                Html.article []
                    [ Html.header []
                        [ Html.h3 []
                            [ Html.text "Descriptions" ]
                        ]
                    , descriptions model.composite.descriptions
                    ]

              else
                Html.text ""
            , if not (List.isEmpty model.composite.links) then
                Html.article []
                    [ Html.header []
                        [ Html.h3 []
                            [ Html.text "Links" ]
                        ]
                    , links model.composite.links
                    ]

              else
                Html.text ""
            , Html.article [ Attr.id "modification-view" ]
                [ Html.header []
                    [ Html.h3 []
                        [ Html.text "Modifications" ]
                    ]
                , modifications model.composite.modifications
                ]
            ]
        ]
