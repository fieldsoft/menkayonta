module Keyed exposing (KeyedDocs(..), SeqData, Value, valueDecoder, valuesToSeqData)

import Dict exposing (Dict)
import Json.Decode as D
import List.Extra as LE
import Maybe.Extra as ME
import Menkayonta as M
import UUID


type alias Value =
    { id : M.Identifier
    , title : String
    , description : String
    , key : String
    , kind : M.SequenceKind
    , doc : M.Value
    }


type alias SeqData =
    { id : String
    , title : String
    , description : String
    , docs : KeyedDocs
    }


type KeyedDocs
    = SKey (Dict String M.Interlinear)
    | IKey (Dict Int M.Interlinear)


valuesToSeqData : List Value -> Maybe SeqData
valuesToSeqData vals =
    let
        firstH : Maybe Value
        firstH =
            List.head vals

        stringKey : Value -> Maybe ( String, M.Interlinear )
        stringKey { key, doc } =
            case ( key, doc ) of
                ( k, M.MyInterlinear int ) ->
                    Just ( k, int )

                _ ->
                    Nothing

        intKey : Value -> Maybe ( Int, M.Interlinear )
        intKey { key, doc } =
            case ( String.toInt key, doc ) of
                ( Just k, M.MyInterlinear int ) ->
                    Just ( k, int )

                _ ->
                    Nothing

        stringKeys : Maybe KeyedDocs
        stringKeys =
            List.map stringKey vals
                |> ME.combine
                |> Maybe.map Dict.fromList
                |> Maybe.map SKey

        intKeys : Maybe KeyedDocs
        intKeys =
            List.map intKey vals
                |> ME.combine
                |> Maybe.map Dict.fromList
                |> Maybe.map IKey

        valsKind : Maybe M.SequenceKind
        valsKind =
            firstH
                |> Maybe.map .kind

        seqData : Value -> KeyedDocs -> SeqData
        seqData first docs =
            { id = M.identifierToString first.id
            , title = first.title
            , description = first.description
            , docs = docs
            }
    in
    case ( firstH, valsKind ) of
        ( Just first, Just M.Integer ) ->
            case intKeys of
                Just docs ->
                    Just (seqData first docs)

                Nothing ->
                    Nothing

        ( Just first, Just M.StringKey ) ->
            case stringKeys of
                Just docs ->
                    Just (seqData first docs)

                Nothing ->
                    Nothing

        _ ->
            Nothing


valueDecoder : D.Decoder Value
valueDecoder =
    let
        decodeId : String -> D.Decoder M.Identifier
        decodeId str =
            case M.stringToIdentifier str of
                Just id ->
                    D.succeed id

                Nothing ->
                    D.fail "The supplied id string was invalid."

        kindHelper : String -> D.Decoder M.SequenceKind
        kindHelper k =
            case k of
                "integer" ->
                    D.succeed M.Integer

                "string" ->
                    D.succeed M.StringKey

                _ ->
                    D.fail <|
                        String.concat
                            [ "Trying to decode a sequence kind '"
                            , k
                            , "' is an invalid kind"
                            ]
    in
    D.map6 Value
        (D.field "id" D.string
            |> D.andThen decodeId
        )
        (D.field "title" D.string)
        (D.field "description" D.string)
        (D.field "key" D.string)
        (D.field "kind" D.string
            |> D.andThen kindHelper
        )
        (D.field "doc" M.decoder)
