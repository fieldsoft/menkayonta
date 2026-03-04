module Display.Note exposing (view)

import Html
import Html.Attributes as Attr
import Menkayonta as M
import Msg exposing (Msg(..))
import Markdown

type alias Model =
    { note : M.Note
    , title : String
    , description : String
    }


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.h1 [ Attr.class "note-title" ] [ Html.text model.title ]
        , Html.p [] [ Html.text model.description ]
        , Markdown.toHtml [] model.note.note
        ]
