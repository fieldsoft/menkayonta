module DativeTypes exposing (convert)

import Json.Decode as D
import Json.Decode.Extra as DE
import Json.Encode as E
import Time
import UUID


type alias SyntacticCategory =
    { id : Int
    , name : String
    }


syntacticCategoryDecoder : D.Decoder SyntacticCategory
syntacticCategoryDecoder =
    D.map2 SyntacticCategory
        (D.field "id" D.int)
        (D.field "name" D.string)


type alias ElicitationMethod =
    { id : Int
    , name : String
    }


elicitationMethodDecoder : D.Decoder ElicitationMethod
elicitationMethodDecoder =
    D.map2 SyntacticCategory
        (D.field "id" D.int)
        (D.field "name" D.string)


type alias Token =
    List SubToken


tokenDecoder : D.Decoder Token
tokenDecoder =
    D.list subTokenDecoder


type alias SubToken =
    { id : Maybe Int
    , code1 : Maybe String
    , code2 : Maybe String
    }


subTokenDecoder : D.Decoder SubToken
subTokenDecoder =
    D.map3 SubToken
        (D.maybe (D.index 0 D.int))
        (D.maybe (D.index 1 D.string))
        (D.maybe (D.index 2 D.string))


type alias Person =
    { id : Int
    , first_name : String
    , last_name : String
    , role : String
    }


personDecoder : D.Decoder Person
personDecoder =
    D.map4 Person
        (D.field "id" D.int)
        (D.field "first_name" D.string)
        (D.field "last_name" D.string)
        (D.field "role" D.string)


type alias Speaker =
    { id : Int
    , first_name : String
    , last_name : String
    , dialect : Maybe String
    }


speakerDecoder : D.Decoder Speaker
speakerDecoder =
    D.map4 Speaker
        (D.field "id" D.int)
        (D.field "first_name" D.string)
        (D.field "last_name" D.string)
        (D.field "dialect" (D.nullable D.string))


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


tagDecoder : D.Decoder Tag
tagDecoder =
    D.map2 Tag
        (D.field "id" D.int)
        (D.field "name" D.string)


testFormEncoder : DativeForm -> E.Value
testFormEncoder tf =
    E.object
        [ ( "_id", E.string ("interlinear::" ++ UUID.toString tf.uuid) )
        , ( "version", E.int 1 )
        , ( "transcription", E.string tf.transcription )
        , ( "phonetic_transcription", E.string tf.phonetic_transcription )
        , ( "morpheme_break", E.string tf.morpheme_break )
        , ( "morpheme_gloss", E.string tf.morpheme_gloss )
        , ( "translations", E.list translationEncoder tf.translations )
        ]


testFormsEncoder : List DativeForm -> E.Value
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
    , date_elicited : Maybe Time.Posix
    , datetime_entered : Time.Posix
    , datetime_modified : Time.Posix
    , syntactic_category_string : Maybe String
    , morpheme_break_ids : Maybe (List Token)
    , morpheme_gloss_ids : Maybe (List Token)
    , break_gloss_category : Maybe String
    , syntax : String
    , semantics : String
    , status : String
    , elicitor : Maybe Person
    , enterer : Maybe Person
    , modifier : Maybe Person
    , verifier : Maybe Person
    , speaker : Maybe Speaker
    , elicitation_method : Maybe ElicitationMethod
    , syntactic_category : Maybe SyntacticCategory
    , source : Maybe String
    , translations : List Translation
    , tags : List Tag
    , files : List String
    }


dativeFormDecoder : D.Decoder DativeForm
dativeFormDecoder =
    D.succeed DativeForm
        |> DE.andMap (D.field "id" D.int)
        |> DE.andMap (D.field "UUID" UUID.jsonDecoder)
        |> DE.andMap (D.field "transcription" D.string)
        |> DE.andMap (D.field "phonetic_transcription" D.string)
        |> DE.andMap (D.field "narrow_phonetic_transcription" D.string)
        |> DE.andMap (D.field "morpheme_break" D.string)
        |> DE.andMap (D.field "morpheme_gloss" D.string)
        |> DE.andMap (D.field "comments" D.string)
        |> DE.andMap (D.field "speaker_comments" D.string)
        |> DE.andMap (D.field "grammaticality" D.string)
        |> DE.andMap (D.field "date_elicited" (D.nullable DE.datetime))
        |> DE.andMap (D.field "datetime_entered" DE.datetime)
        |> DE.andMap (D.field "datetime_modified" DE.datetime)
        |> DE.andMap (D.field "syntactic_category_string" (D.nullable D.string))
        |> DE.andMap (D.field "morpheme_break_ids" (D.nullable (D.list tokenDecoder)))
        |> DE.andMap (D.field "morpheme_gloss_ids" (D.nullable (D.list tokenDecoder)))
        |> DE.andMap (D.field "break_gloss_category" (D.nullable D.string))
        |> DE.andMap (D.field "syntax" D.string)
        |> DE.andMap (D.field "semantics" D.string)
        |> DE.andMap (D.field "status" D.string)
        |> DE.andMap (D.field "elicitor" (D.nullable personDecoder))
        |> DE.andMap (D.field "enterer" (D.nullable personDecoder))
        |> DE.andMap (D.field "modifier" (D.nullable personDecoder))
        |> DE.andMap (D.field "verifier" (D.nullable personDecoder))
        |> DE.andMap (D.field "speaker" (D.nullable speakerDecoder))
        |> DE.andMap (D.field "elicitation_method" (D.nullable elicitationMethodDecoder))
        |> DE.andMap (D.field "syntactic_category" (D.nullable syntacticCategoryDecoder))
        |> DE.andMap (D.field "source" (D.nullable D.string))
        |> DE.andMap (D.field "translations" (D.list translationDecoder))
        |> DE.andMap (D.field "tags" (D.list tagDecoder))
        |> DE.andMap (D.field "files" (D.list D.string))


convert : E.Value -> E.Value
convert vals =
    case D.decodeValue (D.list dativeFormDecoder) vals of
        Err e ->
            E.string <| D.errorToString e

        Ok tfs ->
            testFormsEncoder tfs
