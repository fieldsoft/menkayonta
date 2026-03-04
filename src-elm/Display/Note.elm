module Display.Note exposing (view)

import Html
import Msg exposing (Msg(..))
import Menkayonta as M

type alias Model =
    { note : M.Note
    , title : String
    , description : String
    }
    
view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.h1 [] [ Html.text model.title ]
        , Html.p [] [ Html.text model.description ]
        , Html.pre [] [ Html.text model.note.note ]
        ]
