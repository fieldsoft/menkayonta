module Display.Note exposing (Model, edit, view)

import Html
import Html.Attributes as Attr
import Html.Events as Event
import Markdown exposing (render)
import Menkayonta as M
import Msg exposing (Msg(..))


type alias Model =
    { note : M.Note
    , original : String
    , title : String
    , description : String
    }


view : Model -> Html.Html Msg
view model =
    let
        rendered : Html.Html Msg
        rendered =
            render model.note.note
    in
    Html.div []
        [ Html.nav []
            [ Html.ul []
                [ Html.li []
                    [ Html.a
                        [ Attr.href "#"
                        , Event.onClick EditToggle
                        ]
                        [ Html.text "Edit" ]
                    ]
                ]
            ]
        , Html.h1 [ Attr.class "note-title" ] [ Html.text model.title ]
        , Html.p [] [ Html.text model.description ]
        , rendered
        ]


edit : Model -> Html.Html Msg
edit model =
    Html.div []
        [ Html.nav []
            [ Html.ul []
                [ Html.li []
                    [ Html.a
                        [ Attr.href "#"
                        , Event.onClick EditToggle
                        ]
                        [ Html.text "View" ]
                    ]
                ]
            ]
        , Html.h1 [ Attr.class "note-title" ] [ Html.text model.title ]
        , Html.p [] [ Html.text model.description ]
        , Html.form []
            [ Html.button
                (if model.note.note /= model.original then
                    [ Event.onClick <| SaveNote model.note
                    , Attr.type_ "button"
                    ]

                 else
                    [ Attr.type_ "button"
                    , Attr.disabled True
                    ]
                )
                [ Html.text "Save" ]
            , Html.button
                [ Attr.class "secondary"
                , Attr.type_ "button"
                , Event.onClick <|
                    ChangeNote
                        ("NOTE::"
                            ++ M.identifierToString
                                (M.MyNoteId
                                    model.note.id
                                )
                        )
                        model.original
                ]
                [ Html.text "Cancel" ]
            , Html.textarea
                [ Attr.value model.note.note
                , Attr.class "note-editor"
                , Event.onInput <|
                    ChangeNote
                        ("NOTE::"
                            ++ M.identifierToString
                                (M.MyNoteId
                                    model.note.id
                                )
                        )
                ]
                []
            ]
        ]
