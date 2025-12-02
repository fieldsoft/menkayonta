module DativeTypes exposing (convert)

import Json.Decode as D
import Json.Encode as E
import UUID


type alias Token =
    List (Maybe SubToken)


type alias SubToken =
    { id : Int
    , code1 : String
    , code2 : String
    }


type alias Person =
    { id : Int
    , first_name : String
    , last_name : String
    , role : String
    }


type alias Speaker =
    { id : Int
    , first_name : String
    , last_name : String
    , dialect : String
    }


type alias Translation =
    { id : Int
    , transcription : String
    , grammaticality : String
    }


translationDecoder : D.Decoder Translation
translationDecoder =
    D.map3 Translation
        (D.field "id" D.int)
        (D.field "transcription" D.string)
        (D.field "grammaticality" D.string)


translationEncoder : Translation -> E.Value
translationEncoder tr =
    E.object
        [ ( "id", E.int tr.id )
        , ( "transcription", E.string tr.transcription )
        , ( "grammaticality", E.string tr.grammaticality )
        ]


type alias Tag =
    { id : Int
    , name : String
    }


type alias TestForm =
    { uuid : String
    , transcription : String
    , phonetic_transcription : String
    , morpheme_break : String
    , morpheme_gloss : String
    , translations : List Translation
    }


testFormDecoder : D.Decoder TestForm
testFormDecoder =
    D.map6 TestForm
        (D.field "UUID" D.string)
        (D.field "transcription" D.string)
        (D.field "phonetic_transcription" D.string)
        (D.field "morpheme_break" D.string)
        (D.field "morpheme_gloss" D.string)
        (D.field "translations" <| D.list translationDecoder)


testFormsDecoder : D.Decoder (List TestForm)
testFormsDecoder =
    D.list testFormDecoder


testFormEncoder : TestForm -> E.Value
testFormEncoder tf =
    E.object
        [ ( "_id", E.string ("interlinear::" ++ tf.uuid) )
        , ( "version", E.int 1 )
        , ( "transcription", E.string tf.transcription )
        , ( "phonetic_transcription", E.string tf.phonetic_transcription )
        , ( "morpheme_break", E.string tf.morpheme_break )
        , ( "morpheme_gloss", E.string tf.morpheme_gloss )
        , ( "translations", E.list translationEncoder tf.translations )
        ]


testFormsEncoder : List TestForm -> E.Value
testFormsEncoder tfs =
    E.list testFormEncoder tfs


type alias DativeForm =
    { id : Int
    , uuid : UUID.UUID
    , transcription : String
    , phonetic_transcription : String
    , narrow_phonetic_transcription : String
    , morpheme_break : String
    , morpheme_gloss : String
    , comments : String
    , speaker_comments : String
    , grammaticality : String

    --    , date_elicited : Maybe Date
    --    , datetime_entered : DateTime
    --    , datetime_modified : DateTime
    , syntactic_category_string : String
    , morpheme_break_ids : List Token
    , morpheme_gloss_ids : List Token
    , break_gloss_category : String
    , syntax : String
    , semantics : String
    , status : String
    , elictor : Maybe Person
    , enterer : Person
    , modifier : Person
    , verifier : Maybe Person
    , speaker : Speaker
    , elicitation_method : Maybe String
    , syntactic_category : Maybe String
    , source : Maybe String
    , translation : List Translation
    , tags : List Tag
    , files : List String
    }


convert : E.Value -> E.Value
convert vals =
    case D.decodeValue testFormsDecoder vals of
        Err e ->
            E.string <| D.errorToString e

        Ok tfs ->
            testFormsEncoder tfs
