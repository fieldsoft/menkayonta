module Display.PersonListing exposing (view, Params, viewPerson)

import Html
import Html.Attributes as Attr
import Html.Events as Event
import Menkayonta exposing (Person)
import List.Extra as LE
import Dict
import Url exposing (percentDecode)


type alias Params msg =
    { people : List Person
    , viewEvent : String -> msg
    , editEvent : Person -> msg
    }


view : Params msg -> Html.Html msg
view params =
    Html.ul [ Attr.class "index-listing" ]
        (List.map
            (viewItem params.viewEvent params.editEvent)
            params.people
        )


viewItem :
    (String -> msg)
    -> (Person -> msg)
    -> Person
    -> Html.Html msg
viewItem viewEvent editEvent person =
    Html.li []
        [ viewPerson person
        , Html.div [ Attr.class "perons-controls" ]
            [ Html.a
                [ Attr.href "#"
                , Attr.class "nav-link"
                , Event.onClick (viewEvent person.id)
                ]
                [ Html.text "View" ]
            , Html.a
                [ Attr.href "#"
                , Attr.class "nav-link"
                , Event.onClick (editEvent person)
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
                [ Html.text (percentDecode person.id
                                 |> Maybe.withDefault person.id
                            )
                ]

        nameLines : Html.Html msg
        nameLines =
            Html.ul []
                ( Dict.values person.names
                |> List.map (\n -> Html.li [] [ Html.text n ])
                )
    in
    Html.article []
        [ Html.header [] [ emailLine ]
        , nameLines
        ]


