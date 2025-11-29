port module Converter exposing (main)

import Json.Decode as D
import Json.Encode as E
import Platform
import Random
import UUID


port receivedDativeForms : (E.Value -> msg) -> Sub msg


port sendBulkDocs : E.Value -> Cmd msg


type Msg
    = ReceivedDativeForms E.Value


type alias Model =
    { seeds : UUID.Seeds }


type alias Flags =
    { seed1 : Int
    , seed2 : Int
    , seed3 : Int
    , seed4 : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        seeds =
            { seed1 = Random.initialSeed flags.seed1
            , seed2 = Random.initialSeed flags.seed2
            , seed3 = Random.initialSeed flags.seed3
            , seed4 = Random.initialSeed flags.seed4
            }
    in
    ( { seeds = seeds }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedDativeForms _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    receivedDativeForms ReceivedDativeForms


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


newUuid : UUID.Seeds -> String
newUuid seeds =
    UUID.step seeds |> Tuple.first |> UUID.toString
