module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport)
import Html
import Html.Attributes as Attr
import Html.Events as Event
import Json.Decode as D
import Json.Encode as E
import Task


type alias Model =
    { config : Config
    , error : Maybe Error
    , windowHeight : Int
    }


type Msg
    = SubmittedForm
    | UpdatedInput Field String


type alias StorageItem =
    { key : String
    , value : E.Value
    }


type alias Flags =
    { windowHeight : Int }


type alias Config =
    { state : String }


type Field
    = ProjectName
    | Language


type alias Error =
    { message : String
    , invalid : Bool
    }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { config = { state = "" }
      , error = Nothing
      , windowHeight = flags.windowHeight
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmittedForm ->
            ( model, Cmd.none )

        UpdatedInput _ _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- view model = map (displayView model) model.vids -- which would be a list [ vid1, ..., vidn ]
-- vid : {


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.header [ Attr.class "container-fluid" ]
            [ Html.nav []
                [ Html.ul []
                    [ Html.li [] [ Html.a [ Attr.href "/" ] [ Html.text "Home" ] ] ]
                , Html.ul []
                    [ Html.li [] [ Html.button [] [ Html.text "Save" ] ]
                    , Html.li [] [ Html.button [] [ Html.text "Open" ] ]
                    ]
                ]
            ]
        , Html.main_ [ Attr.class "container-fluid" ] [ Html.h1 [ Attr.class "title" ] [ Html.text "Welcome Here" ] ]
        ]


homeView model =
    Html.div []
        [ Html.header [ Attr.class "container-fluid" ]
            [ Html.nav []
                [ Html.ul []
                    [ Html.li [] [ Html.a [ Attr.href "/" ] [ Html.text "Home" ] ] ]
                , Html.ul []
                    [ Html.li [] [ Html.button [] [ Html.text "Save" ] ]
                    , Html.li [] [ Html.button [] [ Html.text "Open" ] ]
                    ]
                ]
            ]
        , Html.main_ [ Attr.class "container-fluid" ] [ Html.h1 [ Attr.class "title" ] [ Html.text "Welcome" ] ]
        ]
