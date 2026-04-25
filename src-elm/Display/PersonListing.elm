module Display.PersonListing exposing (Model, view, viewPerson)

import Dict
import Html
import Html.Attributes as Attr
import Html.Events as Event
import Menkayonta exposing (Person, DocId(..), Identifier(..), identifierToString)
import Msg exposing (EditType(..))
import UUID
import Url exposing (percentDecode)


type alias Msg =
    Msg.Msg


type alias Model =
    { people : List Person
    , project : UUID.UUID
    }


view : Model-> Html.Html Msg
view model =
    Html.ul [ Attr.class "index-listing" ]
        (List.map
            (viewItem model)
            model.people
        )


viewItem : Model -> Person -> Html.Html Msg
viewItem model person =
    Html.li []
        [ viewPerson person
        , Html.div [ Attr.class "person-controls" ]
            [ Html.a
                [ Attr.href "#"
                , Attr.class "nav-link"
                , PersonId person.id
                    |> MyDocId
                    |> identifierToString
                    |> Msg.OComposite
                    |> Msg.Request model.project
                    |> Msg.UserClick
                    |> Event.onClick
                ]
                [ Html.text "View" ]
            , Html.a
                [ Attr.href "#"
                , Attr.class "nav-link"
                , EPerson model.project person
                    |> Msg.Edit
                    |> Msg.UserClick
                    |> Event.onClick
                ]
                [ Html.text "Edit" ]
            ]
        ]


viewPerson : Person -> Html.Html msg
viewPerson person =
    let
        emailLine : Html.Html msg
        emailLine =
            Html.span [ Attr.class "person-email" ]
                [ Html.text
                    (percentDecode person.id
                        |> Maybe.withDefault person.id
                    )
                ]

        nameLines : Html.Html msg
        nameLines =
            Html.ul []
                (Dict.values person.names
                    |> List.map (\n -> Html.li [] [ Html.text n ])
                )
    in
    Html.article []
        [ Html.header [] [ emailLine ]
        , nameLines
        ]
