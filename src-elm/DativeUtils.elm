module DativeUtils exposing (convert)

import Json.Encode as E

type alias Uuid = String

type alias Date = String

type alias DateTime = String

type alias Token = List (Maybe SubToken)

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

type alias Tag =
    { id : Int
    , name : String
    }

type alias DativeForm =
    { id : Int
    , uuid : Uuid
    , transcription : String
    , phonetic_transcription : String
    , narrow_phonetic_transcription : String
    , morpheme_break : String
    , morpheme_gloss : String
    , comments : String
    , speaker_comments : String
    , grammaticality : String
    , date_elicited : Maybe Date
    , datetime_entered : DateTime
    , datetime_modified : DateTime
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

convert : a -> E.Value
convert _ = E.null
