module Display.InterlinearComposite exposing (view)

import Menkayonta exposing (Interlinear, Composite, Translation)
import Html
import Html.Attributes as Attr
import Html.Events as Event



viewInterlinearOneDoc : M.Composite -> M.Interlinear -> Html.Html Msg
viewInterlinearOneDoc od int =
    Html.div [ Attr.class "docview" ]
        [ Html.h2 []
            [ Html.text "Interlinear Gloss" ]
        , Html.article []
            [ Display.InterlinearListing.viewInterlinear int
            , Html.footer []
                [ M.InterlinearId int.id
                    |> M.MyDocId
                    |> M.identifierToString
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
            [ if not (List.isEmpty od.tags) then
                Html.article []
                    [ Html.header []
                        [ Html.h3 []
                            [ Html.text "Tags" ]
                        ]
                    , viewTags od.tags
                    ]

              else
                Html.text ""
            , if not (List.isEmpty od.properties) then
                Html.article []
                    [ Html.header []
                        [ Html.h3 []
                            [ Html.text "Properties" ]
                        ]
                    , viewProperties od.properties
                    ]

              else
                Html.text ""
            , if not (List.isEmpty od.descriptions) then
                Html.article []
                    [ Html.header []
                        [ Html.h3 []
                            [ Html.text "Descriptions" ]
                        ]
                    , viewDescriptions od.descriptions
                    ]

              else
                Html.text ""
            , Html.article [ Attr.id "modification-view" ]
                [ Html.header []
                    [ Html.h3 []
                        [ Html.text "Modifications" ]
                    ]
                , viewModifications od.modifications
                ]
            ]
        ]
