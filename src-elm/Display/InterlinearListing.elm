module Display.InterlinearListing exposing (Params, view, viewInterlinear)

import Html
import Html.Attributes as Attr
import Html.Events as Event
import Menkayonta exposing (Interlinear, Translation)
import List.Extra as LE
import UUID
import Dict


type alias Params msg =
    { interlinears : List Interlinear
    , viewEvent : UUID.UUID -> msg
    , editEvent : Interlinear -> msg
    }


view : Params msg -> Html.Html msg
view params =
    Html.ul [ Attr.class "all-glosses" ]
        (List.map
            (viewItem params.viewEvent params.editEvent)
            params.interlinears
        )


viewItem :
    (UUID.UUID -> msg)
    -> (Interlinear -> msg)
    -> Interlinear
    -> Html.Html msg
viewItem viewEvent editEvent int =
    Html.li []
        [ viewInterlinear int
        , Html.div [ Attr.class "gloss-controls" ]
            [ Html.a
                [ Attr.href "#"
                , Attr.class "nav-link"
                , Event.onClick (viewEvent int.id)
                ]
                [ Html.text "View" ]
            , Html.a
                [ Attr.href "#"
                , Attr.class "nav-link"
                , Event.onClick (editEvent int)
                ]
                [ Html.text "Edit" ]
            ]
        ]


viewInterlinear : Interlinear -> Html.Html msg
viewInterlinear int =
    let
        srcLine : Html.Html msg
        srcLine =
            Html.span [ Attr.class "gloss-source-text" ]
                [ Html.text (int.ann.judgment ++ " " ++ int.text) ]

        annLines : Html.Html msg
        annLines =
            if int.ann.breaks /= "" then
                viewAnn int.ann.breaks int.ann.glosses

            else
                Html.text ""

        transLine : Translation -> String
        transLine tr =
            String.concat
                [ tr.judgment
                , " "
                , "'"
                , tr.translation
                , "'"
                ]

        transLines : List (Html.Html msg)
        transLines =
            List.map
                (\t ->
                    Html.p []
                        [ Html.text (transLine t) ]
                )
                (Dict.values int.translations)
    in
    Html.article []
        [ Html.header [] [ srcLine ]
        , annLines
        , Html.footer [] transLines
        ]


viewAnn : String -> String -> Html.Html msg
viewAnn brk gls =
    let
        brk_ : List String
        brk_ =
            String.split " " brk

        gls_ : List String
        gls_ =
            if String.isEmpty gls then
                List.repeat (List.length brk_) "—"

            else
                String.split " " gls

        aligned : List ( String, String )
        aligned =
            LE.zip brk_ gls_
    in
    List.map viewGlosses aligned
        |> Html.p [ Attr.class "aligned-glosses" ]


viewGlosses : ( String, String ) -> Html.Html msg
viewGlosses ( a, b ) =
    Html.div [ Attr.class "gloss-column" ]
        [ Html.div [] [ Html.text a ]
        , Html.div [] [ Html.text b ]
        ]
