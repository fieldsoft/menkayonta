port module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport)
import Html
import Html.Attributes as Attr
import Html.Events as Event
import Json.Decode as D
import Json.Encode as E
import Task


port setTitle : String -> Cmd msg

port openFile : () -> Cmd msg

port receivePath : (String -> msg) -> Sub msg

port receiveCount : (Int -> msg) -> Sub msg

                
type alias Model =
    { config : Config
    , error : Maybe Error
    , title : String
    , filePath : String
    , counter : Int
    , windowHeight : Int
    }


type Msg
    = SubmittedForm
    | UpdatedInput Field String
    | UpdateTitle String
    | SetTitle
    | ReceivedPath String
    | OpenFile
    | ReceivedCount Int


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
      , title = ""
      , filePath = "No Path"
      , counter = 0
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

        UpdateTitle s ->
            ( { model | title = s }, Cmd.none )

        SetTitle ->
            ( model, setTitle model.title )

        OpenFile ->
            ( model, openFile () )

        ReceivedPath s ->
            ( { model | filePath = s }, Cmd.none )

        ReceivedCount i ->
            ( { model | counter = model.counter + i }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ receivePath ReceivedPath
              , receiveCount ReceivedCount
              ]



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
        , Html.main_
              [ Attr.class "container-fluid" ]
              [ Html.h1 [ Attr.class "title" ] [ Html.text "rrrrr" ]
              , Html.input
                    [ Event.onInput UpdateTitle
                    , Attr.value model.title
                    ] []
              , Html.button [ Event.onClick SetTitle ] [ Html.text "Set Title" ]
              , Html.hr [] []
              , Html.button [ Event.onClick OpenFile ] [ Html.text "Open File" ]
              , Html.p []
                  [ Html.text "File Path"
                  , Html.strong [] [ Html.text model.filePath ]
                  ]
              , Html.hr [] []
              , Html.p [] [ Html.text (String.fromInt model.counter) ]
              ]
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
