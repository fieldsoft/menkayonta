module SharedTypes exposing (..)

import Json.Decode as D
import Json.Decode.Extra as DE
import Json.Encode as E
import Json.Encode.Extra as EE
import Time


type alias Named =
    { id : Int
    , name : String
    }


type alias Token =
    List SubToken


type alias SubToken =
    { id : Maybe Int
    , code1 : Maybe String
    , code2 : Maybe String
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
    , dialect : Maybe String
    }


type alias Translation =
    { id : Int
    , transcription : String
    , grammaticality : String
    }


type alias Interlinear =
    { id : String
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


namedDecoder : D.Decoder Named
namedDecoder =
    D.map2 Named
        (D.field "id" D.int)
        (D.field "name" D.string)


namedEncoder : Named -> E.Value
namedEncoder named =
    E.object
        [ ( "id", E.int named.id )
        , ( "name", E.string named.name )
        ]


tokenDecoder : D.Decoder Token
tokenDecoder =
    D.list subTokenDecoder


tokenEncoder : List SubToken -> E.Value
tokenEncoder sts =
    E.list subTokenEncoder sts


subTokenEncoder : SubToken -> E.Value
subTokenEncoder st =
    E.object
        [ ( "id", EE.maybe E.int st.id )
        , ( "code1", EE.maybe E.string st.code1 )
        , ( "code2", EE.maybe E.string st.code2 )
        ]


subTokenDecoder : D.Decoder SubToken
subTokenDecoder =
    D.map3 SubToken
        (D.field "id" (D.nullable D.int))
        (D.field "code1" (D.nullable D.string))
        (D.field "code2" (D.nullable D.string))


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


interlinearDecoder : D.Decoder Interlinear
interlinearDecoder =
    D.succeed Interlinear
        |> DE.andMap (D.field "_id" D.string)
        |> DE.andMap (D.field "version" D.int)
        |> DE.andMap (D.field "oldid" D.int)
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
