port module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport)
import Html
import Html.Attributes as Attr
import Html.Events as Event
import Json.Decode as D
import Json.Encode as E
import Task
import Dict exposing (Dict)


port setTitle : String -> Cmd msg

port openFile : () -> Cmd msg

port receivePath : (String -> msg) -> Sub msg

port receiveCount : (Int -> msg) -> Sub msg


type alias Window =
    { title : String
    , vista : String
    }


type alias Vista =
    { project : String
    , kind : String
    , identifier : String
    , content : String
    }

                
type alias Model =
    { config : Config
    , counter : Int
    , columns : List Int
    , rows : Dict Int (List Int)
    , tabs : Dict (Int, Int) (List Int)
    , windows : Dict (Int, Int, Int) Window
    , focused : (Int, Int, Int)
    , openWindows : Dict (Int, Int) Int
    , error : Maybe Error
    , windowHeight : Int
    }


type Msg
    = SubmittedForm
    | UpdatedInput Field String
    | NewTab (String, String)
    | FocusTab (Int, Int, Int)


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


projects : List (String, String)
projects = [ ("first", "xxxx")
           , ("second", "yyyy")
           , ("third", "zzzz")
           ]


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
      , counter = 0
      , columns = []
      , rows = Dict.empty
      , tabs = Dict.empty
      , windows = Dict.empty
      , focused = (-1,-1,-1)
      , openWindows = Dict.empty
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

        NewTab p ->
            let
                newwindow = { title = Tuple.first p
                            , vista = Tuple.second p
                            }
            in
            case model.columns of
                [] ->
                    let
                        c = model.counter
                        newmodel =
                            { model | counter = c + 1
                            , columns = [c]
                            , rows = Dict.singleton c [c]
                            , tabs = Dict.singleton (c,c) [c]
                            , windows = Dict.singleton (c,c,c) newwindow
                            , openWindows = Dict.singleton (c,c) c
                            , focused = (c,c,c)
                            }
                    in
                    ( newmodel, Cmd.none )

                _ ->
                    let
                        c = model.counter
                        (col, row, _) = model.focused
                        tabs = Maybe.withDefault [] (Dict.get (col, row) model.tabs) 
                        newmodel =
                            { model | counter = c + 1
                            , tabs = Dict.insert (col, row) (c :: tabs) model.tabs
                            , windows = Dict.insert (col, row, c) newwindow
                                        model.windows
                            , openWindows = Dict.insert (col, row) c model.openWindows
                            , focused = (col, row, c)
                            }
                    in
                    ( newmodel, Cmd.none )

        FocusTab (col, row, tab) ->
            let
                newmodel =
                    { model | focused = (col, row, tab)
                    , openWindows = Dict.insert (col, row) tab model.openWindows
                    }
            in
            ( newmodel, Cmd.none ) 


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- view model = map (displayView model) model.vids -- which would be a list [ vid1, ..., vidn ]
-- vid : {

roleAttr : String -> Html.Attribute msg
roleAttr role =
    Attr.attribute "role" role


view : Model -> Html.Html Msg
view model =
    Html.main_ [ Attr.class "grid-with-side" ]
        [ Html.aside [ Attr.class "side" ]
              [ Html.nav []
                    [ Html.h2 [] [ Html.text "Projects" ]
                    , Html.ul [] (viewProjects model projects)
                    ]
              ] 
        , case model.columns of
              [] ->
                  Html.div
                      [ Attr.class "container-fluid" ]
                      [ Html.h1 [ Attr.class "title" ] [ Html.text "rrrrroar" ] ]

              cols ->
                  Html.div
                      [ Attr.class "container-fluid grid" ]
                      (List.map (viewColumn model) cols)
        ]


viewColumn : Model -> Int -> Html.Html Msg
viewColumn model col =
    Html.div [] (List.map (viewRow model col)
                     (Maybe.withDefault [] <| Dict.get col model.rows))


viewRow : Model -> Int -> Int -> Html.Html Msg
viewRow model col row =
    Html.div [ Dict.get col model.rows
                   |> Maybe.withDefault [0]
                   |> List.length
                   |> (//) model.windowHeight
                   |> Attr.height
             ] [ Html.div [ roleAttr "group" ]
                     (List.map (viewTabHeader model col row)
                          (Maybe.withDefault [] <| Dict.get (col, row) model.tabs))
               , Html.div []
                     (List.map (viewTab model col row)
                          (Maybe.withDefault [] <| Dict.get (col, row) model.tabs))
               ]


viewTabHeader : Model -> Int -> Int -> Int -> Html.Html Msg
viewTabHeader model col row tab =
    Html.button [ Attr.classList
                      [ ( "outline secondary", (col, row, tab) /= model.focused) ]
                , Event.onClick (FocusTab (col, row, tab))
                ]
        [ Dict.get (col, row, tab) model.windows
              |> Maybe.withDefault { title = "Error"
                                   , vista = "Vista not found"
                                   }
              |> .title
              |> Html.text
        ]


viewTab : Model -> Int -> Int -> Int -> Html.Html Msg
viewTab model col row tab =
    Html.div [ Attr.classList
                   [ ( "focused", (col, row, tab) == model.focused)
                   , ( "hidden", Dict.get (col, row) model.openWindows
                     |> Maybe.withDefault -1
                     |> (/=) tab )
                   ]
             ]
        [ Dict.get (col, row, tab) model.windows
              |> Maybe.withDefault { title = "Error"
                                   , vista = "Vista not found"
                                   }
              |> .vista
              |> Html.text
        ]


viewProjects : Model -> List (String, String) -> List (Html.Html Msg)
viewProjects model ps =
    List.map (\p -> Html.li []
                  [ Html.a
                        [ Attr.href ("#" ++ (Tuple.second p))
                        , Event.onClick (NewTab p)
                        ]
                        [ Html.text (Tuple.first p) ] ]
             ) ps
