module Display.SequenceListing exposing (Model, Msg, view, viewSequence)

import Dict
import Html
import Html.Attributes as Attr
import Html.Events as Event
import Menkayonta exposing (Sequence, DocId(..), Identifier(..), identifierToString)
import Msg
import UUID
import Url exposing (percentDecode)


type alias Msg =
    Msg.Msg


type alias Model =
    { sequences : List Sequence
    , project : UUID.UUID
    }


view : Model -> Html.Html Msg
view model =
    Html.ul [ Attr.class "index-listing" ]
        (List.map
            (viewItem model)
            model.sequences
        )


viewItem : Model -> Sequence -> Html.Html Msg
viewItem model sequence =
    Html.li []
        [ viewSequence sequence
        , Html.div [ Attr.class "sequence-controls" ]
            [ Html.a
                [ Attr.href "#"
                , Attr.class "nav-link"
                , SequenceId sequence.id
                    |> MyDocId
                    |> identifierToString
                    |> Msg.OComposite
                    |> Msg.Request model.project
                    |> Msg.UserClick
                    |> Event.onClick
                ]
                [ Html.text "View Metadata" ]
            -- , Html.a
            --     [ Attr.href "#"
            --     , Attr.class "nav-link"
            --     , SequenceId sequence.id
            --         |> MyDocId
            --         |> identifierToString
            --         |> Msg.OSequence
            --         |> Msg.Request model.project
            --         |> Msg.UserClick
            --         |> Event.onClick
            --     ]
            --     [ Html.text "View Glosses" ]
            -- , Html.a
            --     [ Attr.href "#"
            --     , Attr.class "nav-link"
            --     , Msg.EditSequence model.project sequence
            --         |> Msg.UserClick
            --         |> Event.onClick
            --     ]
            --     [ Html.text "Edit" ]
            ]
        ]


viewSequence : Sequence -> Html.Html Msg
viewSequence sequence =
    let
        titleLine : Html.Html msg
        titleLine =
            Html.span [ Attr.class "sequence-title" ]
                [ Html.text sequence.title ]

        descriptionLine : Html.Html msg
        descriptionLine =
            Html.p [] [ Html.text sequence.description ]
    in
    Html.article []
        [ Html.header [] [ titleLine ]
        , descriptionLine
        ]
