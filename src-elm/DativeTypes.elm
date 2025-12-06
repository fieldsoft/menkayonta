module DativeTypes exposing (convert)

import Iso8601 as Iso
import Json.Decode as D
import Json.Decode.Extra as DE
import Json.Encode as E
import Json.Encode.Extra as EE
import Time
import UUID


type alias Named =
    { id : Int
    , name : String
    }


namedDecoder : D.Decoder Named
namedDecoder =
    D.map2 Named
        (D.field "id" D.int)
        (D.field "name" D.string)


namedEncoder : Named -> E.Value
namedEncoder named =
    E.object
        [ ( "id", E.int named.id )
        , ( "named", E.string named.name )
        ]


type alias Token =
    List SubToken


tokenDecoder : D.Decoder Token
tokenDecoder =
    D.list subTokenDecoder


tokenEncoder : List SubToken -> E.Value
tokenEncoder sts =
    E.list subTokenEncoder sts


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


subTokenEncoder : SubToken -> E.Value
subTokenEncoder st =
    E.object
        [ ( "id", EE.maybe E.int st.id )
        , ( "code1", EE.maybe E.string st.code1 )
        , ( "code2", EE.maybe E.string st.code2 )
        ]


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


personEncoder : Person -> E.Value
personEncoder person =
    E.object
        [ ( "id", E.int person.id )
        , ( "first_name", E.string person.first_name )
        , ( "last_name", E.string person.last_name )
        , ( "role", E.string person.role )
        ]


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


speakerEncoder : Speaker -> E.Value
speakerEncoder speaker =
    E.object
        [ ( "id", E.int speaker.id )
        , ( "first_name", E.string speaker.first_name )
        , ( "last_name", E.string speaker.last_name )
        , ( "dialect", EE.maybe E.string speaker.dialect )
        ]


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


dativeFormEncoder : DativeForm -> E.Value
dativeFormEncoder df =
    E.object
        [ ( "_id", E.string ("interlinear/" ++ UUID.toString df.uuid) )
        , ( "version", E.int 1 )
        , ( "transcription", E.string df.transcription )
        , ( "phonetic_transcription", E.string df.phonetic_transcription )
        , ( "narrow_phonetic_transcription", E.string df.narrow_phonetic_transcription )
        , ( "morpheme_break", E.string df.morpheme_break )
        , ( "morpheme_gloss", E.string df.morpheme_gloss )
        , ( "comments", E.string df.comments )
        , ( "speaker_comments", E.string df.speaker_comments )
        , ( "grammaticality", E.string df.grammaticality )
        , ( "date_elicited", EE.maybe Iso.encode df.date_elicited )
        , ( "datetime_entered", Iso.encode df.datetime_entered )
        , ( "datetime_modified", Iso.encode df.datetime_modified )
        , ( "syntactic_category_string", EE.maybe E.string df.syntactic_category_string )
        , ( "morpheme_break_ids", EE.maybe (E.list tokenEncoder) df.morpheme_break_ids )
        , ( "morpheme_gloss_ids", EE.maybe (E.list tokenEncoder) df.morpheme_gloss_ids )
        , ( "break_gloss_category", EE.maybe E.string df.break_gloss_category )
        , ( "syntax", E.string df.syntax )
        , ( "semantics", E.string df.semantics )
        , ( "status", E.string df.status )
        , ( "elicitor", EE.maybe personEncoder df.elicitor )
        , ( "enterer", EE.maybe personEncoder df.enterer )
        , ( "modifier", EE.maybe personEncoder df.modifier )
        , ( "verifier", EE.maybe personEncoder df.verifier )
        , ( "speaker", EE.maybe speakerEncoder df.speaker )
        , ( "elicitation_method", EE.maybe namedEncoder df.elicitation_method )
        , ( "syntactic_category", EE.maybe namedEncoder df.syntactic_category )
        , ( "source", EE.maybe E.string df.source )
        , ( "translations", E.list translationEncoder df.translations )
        , ( "tags", E.list namedEncoder df.tags )
        , ( "files", E.list E.string df.files )
        ]


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
    , elicitation_method : Maybe Named
    , syntactic_category : Maybe Named
    , source : Maybe String
    , translations : List Translation
    , tags : List Named
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
        |> DE.andMap (D.field "elicitation_method" (D.nullable namedDecoder))
        |> DE.andMap (D.field "syntactic_category" (D.nullable namedDecoder))
        |> DE.andMap (D.field "source" (D.nullable D.string))
        |> DE.andMap (D.field "translations" (D.list translationDecoder))
        |> DE.andMap (D.field "tags" (D.list namedDecoder))
        |> DE.andMap (D.field "files" (D.list D.string))


convert : E.Value -> E.Value
convert vals =
    case D.decodeValue (D.list dativeFormDecoder) vals of
        Err e ->
            E.string <| D.errorToString e

        Ok tfs ->
            E.list dativeFormEncoder tfs
