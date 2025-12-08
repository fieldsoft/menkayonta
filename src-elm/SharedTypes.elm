module SharedTypes exposing (..)

import Json.Decode as D
import Json.Decode.Extra as DE
import Json.Encode as E
import Json.Encode.Extra as EE
import UUID exposing (UUID)
import Time


type alias DativeNamed =
    { id : Int
    , name : String
    }


type alias DativeToken =
    List DativeSubToken


type alias DativeSubToken =
    { id : Maybe Int
    , code1 : Maybe String
    , code2 : Maybe String
    }


type alias DativePerson =
    { id : Int
    , first_name : String
    , last_name : String
    , role : String
    }


type alias DativeSpeaker =
    { id : Int
    , first_name : String
    , last_name : String
    , dialect : Maybe String
    }


type alias DativeTranslation =
    { id : Int
    , transcription : String
    , grammaticality : String
    }

    
type alias DativeInterlinear =
    { id : UUID
    , version : Int
    , oldid : Int
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
    , morpheme_break_ids : Maybe (List DativeToken)
    , morpheme_gloss_ids : Maybe (List DativeToken)
    , break_gloss_category : Maybe String
    , syntax : String
    , semantics : String
    , status : String
    , elicitor : Maybe DativePerson
    , enterer : Maybe DativePerson
    , modifier : Maybe DativePerson
    , verifier : Maybe DativePerson
    , speaker : Maybe DativeSpeaker
    , elicitation_method : Maybe DativeNamed
    , syntactic_category : Maybe DativeNamed
    , source : Maybe String
    , translations : List DativeTranslation
    , tags : List DativeNamed
    , files : List String
    }


dativeNamedDecoder : D.Decoder DativeNamed
dativeNamedDecoder =
    D.map2 DativeNamed
        (D.field "id" D.int)
        (D.field "name" D.string)


dativeNamedEncoder : DativeNamed -> E.Value
dativeNamedEncoder named =
    E.object
        [ ( "id", E.int named.id )
        , ( "name", E.string named.name )
        ]


dativeTokenDecoder : D.Decoder DativeToken
dativeTokenDecoder =
    D.list dativeSubTokenDecoder


dativeTokenEncoder : List DativeSubToken -> E.Value
dativeTokenEncoder sts =
    E.list dativeSubTokenEncoder sts


dativeSubTokenEncoder : DativeSubToken -> E.Value
dativeSubTokenEncoder st =
    E.object
        [ ( "id", EE.maybe E.int st.id )
        , ( "code1", EE.maybe E.string st.code1 )
        , ( "code2", EE.maybe E.string st.code2 )
        ]


dativeSubTokenDecoder : D.Decoder DativeSubToken
dativeSubTokenDecoder =
    D.map3 DativeSubToken
        (D.field "id" (D.nullable D.int))
        (D.field "code1" (D.nullable D.string))
        (D.field "code2" (D.nullable D.string))


dativePersonDecoder : D.Decoder DativePerson
dativePersonDecoder =
    D.map4 DativePerson
        (D.field "id" D.int)
        (D.field "first_name" D.string)
        (D.field "last_name" D.string)
        (D.field "role" D.string)


dativePersonEncoder : DativePerson -> E.Value
dativePersonEncoder person =
    E.object
        [ ( "id", E.int person.id )
        , ( "first_name", E.string person.first_name )
        , ( "last_name", E.string person.last_name )
        , ( "role", E.string person.role )
        ]


dativeSpeakerDecoder : D.Decoder DativeSpeaker
dativeSpeakerDecoder =
    D.map4 DativeSpeaker
        (D.field "id" D.int)
        (D.field "first_name" D.string)
        (D.field "last_name" D.string)
        (D.field "dialect" (D.nullable D.string))


dativeSpeakerEncoder : DativeSpeaker -> E.Value
dativeSpeakerEncoder speaker =
    E.object
        [ ( "id", E.int speaker.id )
        , ( "first_name", E.string speaker.first_name )
        , ( "last_name", E.string speaker.last_name )
        , ( "dialect", EE.maybe E.string speaker.dialect )
        ]


dativeTranslationDecoder : D.Decoder DativeTranslation
dativeTranslationDecoder =
    D.map3 DativeTranslation
        (D.field "id" D.int)
        (D.field "transcription" D.string)
        (D.field "grammaticality" D.string)


dativeTranslationEncoder : DativeTranslation -> E.Value
dativeTranslationEncoder tr =
    E.object
        [ ( "id", E.int tr.id )
        , ( "transcription", E.string tr.transcription )
        , ( "grammaticality", E.string tr.grammaticality )
        ]


-- dativeInterlinearDecoder : D.Decoder DativeInterlinear
-- dativeInterlinearDecoder =
--     D.succeed DativeInterlinear
--         |> DE.andMap (D.field "_id" UUID.decode)
--         |> DE.andMap (D.field "version" D.int)
--         |> DE.andMap (D.field "oldid" D.int)
--         |> DE.andMap (D.field "transcription" D.string)
--         |> DE.andMap (D.field "phonetic_transcription" D.string)
--         |> DE.andMap (D.field "narrow_phonetic_transcription" D.string)
--         |> DE.andMap (D.field "morpheme_break" D.string)
--         |> DE.andMap (D.field "morpheme_gloss" D.string)
--         |> DE.andMap (D.field "comments" D.string)
--         |> DE.andMap (D.field "speaker_comments" D.string)
--         |> DE.andMap (D.field "grammaticality" D.string)
--         |> DE.andMap (D.field "date_elicited" (D.nullable DE.datetime))
--         |> DE.andMap (D.field "datetime_entered" DE.datetime)
--         |> DE.andMap (D.field "datetime_modified" DE.datetime)
--         |> DE.andMap (D.field "syntactic_category_string" (D.nullable D.string))
--         |> DE.andMap (D.field "morpheme_break_ids" (D.nullable (D.list dativeTokenDecoder)))
--         |> DE.andMap (D.field "morpheme_gloss_ids" (D.nullable (D.list dativeTokenDecoder)))
--         |> DE.andMap (D.field "break_gloss_category" (D.nullable D.string))
--         |> DE.andMap (D.field "syntax" D.string)
--         |> DE.andMap (D.field "semantics" D.string)
--         |> DE.andMap (D.field "status" D.string)
--         |> DE.andMap (D.field "elicitor" (D.nullable dativePersonDecoder))
--         |> DE.andMap (D.field "enterer" (D.nullable dativePersonDecoder))
--         |> DE.andMap (D.field "modifier" (D.nullable dativePersonDecoder))
--         |> DE.andMap (D.field "verifier" (D.nullable dativePersonDecoder))
--         |> DE.andMap (D.field "speaker" (D.nullable dativeSpeakerDecoder))
--         |> DE.andMap (D.field "elicitation_method" (D.nullable dativeNamedDecoder))
--         |> DE.andMap (D.field "syntactic_category" (D.nullable dativeNamedDecoder))
--         |> DE.andMap (D.field "source" (D.nullable D.string))
--         |> DE.andMap (D.field "translations" (D.list dativeTranslationDecoder))
--         |> DE.andMap (D.field "tags" (D.list dativeNamedDecoder))
--         |> DE.andMap (D.field "files" (D.list D.string))
