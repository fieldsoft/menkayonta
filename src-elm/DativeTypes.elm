module DativeTypes exposing (convert)

import Iso8601 as Iso
import Json.Decode as D
import Json.Decode.Extra as DE
import Json.Encode as E
import Json.Encode.Extra as EE
import Time
import UUID
import SharedTypes as ST




tokenDecoder : D.Decoder ST.Token
tokenDecoder =
    D.list subTokenDecoder


subTokenDecoder : D.Decoder ST.SubToken
subTokenDecoder =
    D.map3 ST.SubToken
        (D.maybe (D.index 0 D.int))
        (D.maybe (D.index 1 D.string))
        (D.maybe (D.index 2 D.string))


dativeToInterlinear1Encoder : DativeForm -> E.Value
dativeToInterlinear1Encoder df =
    E.object
        [ ( "_id", E.string ("interlinear/" ++ UUID.toString df.uuid) )
        , ( "version", E.int 1 )
        , ( "oldid", E.int df.id )
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
        , ( "morpheme_break_ids", EE.maybe (E.list ST.tokenEncoder) df.morpheme_break_ids )
        , ( "morpheme_gloss_ids", EE.maybe (E.list ST.tokenEncoder) df.morpheme_gloss_ids )
        , ( "break_gloss_category", EE.maybe E.string df.break_gloss_category )
        , ( "syntax", E.string df.syntax )
        , ( "semantics", E.string df.semantics )
        , ( "status", E.string df.status )
        , ( "elicitor", EE.maybe ST.personEncoder df.elicitor )
        , ( "enterer", EE.maybe ST.personEncoder df.enterer )
        , ( "modifier", EE.maybe ST.personEncoder df.modifier )
        , ( "verifier", EE.maybe ST.personEncoder df.verifier )
        , ( "speaker", EE.maybe ST.speakerEncoder df.speaker )
        , ( "elicitation_method", EE.maybe ST.namedEncoder df.elicitation_method )
        , ( "syntactic_category", EE.maybe ST.namedEncoder df.syntactic_category )
        , ( "source", EE.maybe E.string df.source )
        , ( "translations", E.list ST.translationEncoder df.translations )
        , ( "tags", E.list ST.namedEncoder df.tags )
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
    , morpheme_break_ids : Maybe (List ST.Token)
    , morpheme_gloss_ids : Maybe (List ST.Token)
    , break_gloss_category : Maybe String
    , syntax : String
    , semantics : String
    , status : String
    , elicitor : Maybe ST.Person
    , enterer : Maybe ST.Person
    , modifier : Maybe ST.Person
    , verifier : Maybe ST.Person
    , speaker : Maybe ST.Speaker
    , elicitation_method : Maybe ST.Named
    , syntactic_category : Maybe ST.Named
    , source : Maybe String
    , translations : List ST.Translation
    , tags : List ST.Named
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
        |> DE.andMap (D.field "elicitor" (D.nullable ST.personDecoder))
        |> DE.andMap (D.field "enterer" (D.nullable ST.personDecoder))
        |> DE.andMap (D.field "modifier" (D.nullable ST.personDecoder))
        |> DE.andMap (D.field "verifier" (D.nullable ST.personDecoder))
        |> DE.andMap (D.field "speaker" (D.nullable ST.speakerDecoder))
        |> DE.andMap (D.field "elicitation_method" (D.nullable ST.namedDecoder))
        |> DE.andMap (D.field "syntactic_category" (D.nullable ST.namedDecoder))
        |> DE.andMap (D.field "source" (D.nullable D.string))
        |> DE.andMap (D.field "translations" (D.list ST.translationDecoder))
        |> DE.andMap (D.field "tags" (D.list ST.namedDecoder))
        |> DE.andMap (D.field "files" (D.list D.string))


convert : E.Value -> E.Value
convert vals =
    case D.decodeValue (D.list dativeFormDecoder) vals of
        Err e ->
            E.string <| D.errorToString e

        Ok tfs ->
            E.list dativeToInterlinear1Encoder tfs
