port module Converter exposing (main)

import Json.Decode as D
import Json.Encode as E
import Platform
import Random
import UUID
import DativeTypes


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


type alias TestData =
    { uuid : UUID.UUID
    , doctype : String
    , version : Int
    , content : String
    }


type alias Job =
    { project : String
    , payload : E.Value
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
        ReceivedDativeForms job ->
            -- let
            --     ( uuid, seeds ) =
            --         UUID.step model.seeds

            --     td =
            --         [ { uuid = uuid
            --           , doctype = "test"
            --           , version = 1
            --           , content = "test content"
            --           }
            --         ]

            --     tdval =
            --         E.list testDataEncoder td
            -- in
            case D.decodeValue jobDecoder job of
                Err _ ->
                    ( model, Cmd.none )

                Ok j ->
                    let
                        jval =
                            jobEncoder
                                { project = j.project
                                , payload = DativeTypes.convert j.payload
                                }
                    in
                    ( model
                    , sendBulkDocs jval
                    )


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


jobDecoder : D.Decoder Job
jobDecoder =
    D.map2 Job
        (D.field "project" D.string)
        (D.field "payload" D.value)


jobEncoder : Job -> E.Value
jobEncoder job =
    E.object
        [ ( "project", E.string job.project )
        , ( "payload", job.payload )
        ]


testDataEncoder : TestData -> E.Value
testDataEncoder td =
    E.object
        [ ( "_id"
          , E.string (String.join "/" [ td.doctype, UUID.toString td.uuid ])
          )
        , ( "version", E.int td.version )
        , ( "content", E.string td.content )
        ]
