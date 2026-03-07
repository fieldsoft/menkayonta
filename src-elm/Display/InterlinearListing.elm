module Display.InterlinearListing exposing
    ( Model
    , Msg
    , view
    , viewInterlinear
    )

import Dict
import Html
import Html.Attributes as Attr
import Html.Events as Event
import List.Extra as LE
import Menkayonta
    exposing
        ( DocId(..)
        , Identifier(..)
        , Interlinear
        , Translation
        , Value(..)
        , identifierToString
        )
import Msg
import UUID


type alias Msg =
    Msg.Msg


type alias Model =
    { interlinears : List Interlinear
    , project : UUID.UUID
    }


view : Model -> Html.Html Msg
view model =
    Html.ul [ Attr.class "index-listing" ]
        (List.map
            (viewItem model)
            model.interlinears
        )


viewItem : Model -> Interlinear -> Html.Html Msg
viewItem model int =
    Html.li []
        [ viewInterlinear int
        , Html.div [ Attr.class "gloss-controls" ]
            [ Html.a
                [ Attr.href "#"
                , Attr.class "nav-link"
                , InterlinearId int.id
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
                , Msg.EditInterlinear model.project int
                    |> Msg.UserClick
                    |> Event.onClick
                ]
                [ Html.text "Edit" ]
            ]
        ]


viewInterlinear : Interlinear -> Html.Html Msg
viewInterlinear int =
    let
        srcLine : Html.Html Msg
        srcLine =
            Html.span [ Attr.class "gloss-source-text" ]
                [ Html.text (int.ann.judgment ++ " " ++ int.text) ]

        annLines : Html.Html Msg
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

        transLines : List (Html.Html Msg)
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


viewAnn : String -> String -> Html.Html Msg
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


viewGlosses : ( String, String ) -> Html.Html Msg
viewGlosses ( a, b ) =
    Html.div [ Attr.class "gloss-column" ]
        [ Html.div [] [ Html.text a ]
        , Html.div [] [ Html.text b ]
        ]
