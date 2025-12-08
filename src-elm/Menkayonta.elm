module Menkayonta exposing (..)

import Json.Decode as D
import Json.Decode.Extra as DE
import Json.Encode as E
import Json.Encode.Extra as EE
import Maybe.Extra as ME
import Dict exposing (Dict)
import Url exposing (Url)
import Url.Parser as U exposing ((</>), (<?>))
import Url.Parser.Query as Q
import Url.Builder as B
import UUID exposing (UUID)
import Time
import SharedTypes as ST


type Value
    = MyPerson Person
    | MyTag Tag
    | MyProperty Property
    | MyUtility Utility
    | MyModification Modification
    | MyDescription Description
    | MyInterlinear Interlinear
--    | MyDativeInterlinear DativeInterlinear


type Identifier
    = MyPersonId UUID
    | MyTagId TagId
    | MyDescriptionId DescriptionId
    | MyPropertyId PropertyId
    | MyUtilityId UtilityId
    | MyModificationId ModificationId
    | MyInterlinearId UUID
--    | MyDativeInterlinearId UUID


type alias Translation =
    { translation : String
    , judgment : String
    }


type alias Annotations =
    { breaks : String
    , glosses : String
    , phonemic : String
    , judgment : String
    }
                 

type alias Interlinear =
    { id : UUID
    , rev : Maybe String
    , version : Int
    , text : String
    , annotations : Annotations
    , translations : Dict Int Translation
    }


type alias DescriptionId =
    { kind : String
    , docid : Identifier
    , fragment : Maybe String
    }


type alias Description =
    { id : DescriptionId
    , rev : Maybe String
    , version : Int
    , value : E.Value
    }


type alias TagId =
    { kind : String
    , docid : String
    , fragment : Maybe String
    }
    

type alias Tag =
    { id : TagId
    , rev : Maybe String
    , version : String
    }


type alias PropertyId =
    { kind : String
    , value : String
    , docid : String
    , fragment : Maybe String
    }
    

type alias Property =
    { id : PropertyId
    , rev : Maybe String
    , version : String
    }


type alias Person =
    { id : UUID
    , rev : String
    , version : Int
    , email : String
    , names : Dict Int String
    }


type alias ModInfo =
    { docid : String
    , time : Time.Posix
    , personid : String
    }
    
    
type alias ModificationId =
    { kind : String
    , info : ModInfo
    , fragment : Maybe String
    }

    
type alias Modification =
    { id : ModificationId
    , rev : String
    , version : Int
    , docversion : Int
    , docstate : E.Value
    }

        
type alias UtilityId =
    { kind : String
    , docid : String
    }
    

type alias Utility =
    { id : UtilityId
    , rev : String
    , version : Int
    , value : E.Value
    }


type alias DativeUtilityValue =
    { version : Int
    , syntactic_category_string : Maybe String
    , morpheme_break_ids : Maybe (List ST.DativeToken)
    , morpheme_gloss_ids : Maybe (List ST.DativeToken)
    , break_gloss_category : Maybe String
    }


uUUID : U.Parser (UUID -> UUID) UUID
uUUID =
    U.custom "UUID" (\s -> UUID.fromString s |> Result.toMaybe)


qTime : String -> Q.Parser (Maybe (Time.Posix))
qTime query =
    Q.custom query (\s -> List.head s
                   |> Maybe.andThen String.toInt
                   |> Maybe.map Time.millisToPosix
                   )


personIdParser : U.Parser (UUID -> UUID) UUID
personIdParser =
    U.s "person" </> uUUID


interlinearIdParser : U.Parser (UUID -> UUID) UUID
interlinearIdParser =
    U.s "interlinear" </> uUUID


-- tagIdParser : U.Parser (TagId -> a) a
-- tagIdParser =
--     U.s "tag" </> U.string <?> Q.string "id" </> U.fragment


-- descriptionIdParser : U.Parser (DescriptionId -> a) a
-- descriptionIdParser =
--     U.map DescriptionId (U.s "description" </> U.string <?> Q.string "id" </> U.fragment)


-- propertyIdParser : U.Parser (PropertyId -> a) a
-- propertyIdParser =
--     U.s "property" </> U.string </> U.string <?> Q.string "id" </> U.fragment


-- utilityIdParser : U.Parser (UtilityId -> a) a
-- utilityIdParser =
--     U.s "utility" </> U.string <?> Q.string "id"


-- modificationIdParser : U.Parser (ModificationId -> a) a
-- modificationIdParser =
--     U.s "modification" </> U.string <?> Q.map3 ModInfo (Q.string "id") (qTime "time") (Q.string "person") </> U.fragment

        
-- identifierParser : U.Parser (Identifier -> a) a
-- identifierParser =
--     U.oneOf
--         [ U.map MyPersonId personIdParser
--         , U.map MyInterlinearId interlinearIdParser
--         , U.map MyTagId tagIdParser
--         , U.map MyDescriptionId descriptionIdParser
--         , U.map MyPropertyId propertyIdParser
--         , U.map MyUtilityId utilityIdParser
--         , U.map MyModificationId modificationIdParser
-- --        , U.map MyDativeInterlinearId (U.s "dativeLegacy" </> uUUID)
--         ]


-- valueDecoder_ : Identifier -> D.Decoder Value
-- valueDecoder_ id =
--     case id of
--         -- MyDativeInterlinearId id_ ->
--         --     dativeInterlinearDecoder_ id_

--         MyPersonId id_ ->
--             personDecoder_ id_
                
--         MyTagId id_ ->
--             tagDecoder_ id_
                
--         MyDescriptionId id_ ->
--             descriptionDecoder_ id_
                
--         MyPropertyId id_ ->
--             propertyDecoder_ id_
                
--         MyUtilityId id_ ->
--             utilityDecoder_ id_
                
--         MyModificationId id_ ->
--             modificationDecoder_ id_
                
--         MyInterlinearId id_ ->
--             interlinearDecoder_ id_
        

-- valueDecoder : D.Decoder Value
-- valueDecoder =
--     parserDecoder identifierParser valueDecoder_


parserDecoder : (U.Parser (UUID -> UUID) UUID) -> (UUID -> D.Decoder Interlinear) -> D.Decoder Interlinear
parserDecoder parser decoder =
    D.field "_id" D.string
        |> D.map ((++) "http://example.com/")
        |> D.map Url.fromString
        |> D.andThen (ME.unwrap (D.fail "Cannot Convert to Url") D.succeed) 
        |> D.map (U.parse parser)
        |> D.andThen (ME.unwrap (D.fail "Bad Identifier") decoder)
    

interlinearDecoder : D.Decoder Interlinear
interlinearDecoder =
    parserDecoder interlinearIdParser interlinearDecoder_
       

interlinearDecoder_ : UUID -> D.Decoder Interlinear
interlinearDecoder_ id =
    D.map6 Interlinear
        (D.succeed id)
        (D.field "_rev" <| D.nullable D.string)
        (D.field "version" D.int)
        (D.field "text" D.string)
        (D.field "annotations" annotationsDecoder)
        (D.field "translations" (DE.dict2 D.int translationDecoder))


annotationsDecoder : D.Decoder Annotations
annotationsDecoder =
    D.map4 Annotations
        (D.field "breaks" D.string)
        (D.field "glosses" D.string)
        (D.field "phonemic" D.string)
        (D.field "judgment" D.string)


translationDecoder : D.Decoder Translation
translationDecoder =
    D.map2 Translation
        (D.field "translation" D.string)
        (D.field "judgment" D.string)


-- personDecoder : D.Decoder Person
-- personDecoder =
--     parserDecoder personIdParser personDecoder_
       

-- personDecoder_ : UUID -> D.Decoder Interlinear
-- personDecoder_ id =
--     D.map5 Person
--         (D.succeed id)
--         (D.field "_rev" <| D.nullable D.string)
--         (D.field "version" D.int)
--         (D.field "email" D.string)
--         (D.field "names" (D.dict D.string))


-- descriptionDecoder : D.Decoder Description
-- descriptionDecoder =
--     parserDecoder descriptionIdParser descriptionDecoder_


-- descriptionDecoder_ : DescriptionId -> D.Decoder Description
-- descriptionDecoder_ id =
--     D.map4 Description
--         (D.succeed id)
--         (D.field "_rev" <| D.nullable D.string)
--         (D.field "version" D.int)
--         (D.field "value" D.value)


-- tagDecoder : D.Decoder Tag
-- tagDecoder =
--     parserDecoder tagIdParser tagDecoder_


-- tagDecoder_ : TagId -> D.Decoder Tag
-- tagDecoder_ id =
--     D.map3 Tag
--         (D.succeed id)
--         (D.field "_rev" <| D.nullable D.string)
--         (D.field "version" D.int)

            
-- propertyDecoder : D.Decoder Property
-- propertyDecoder =
--     parserDecoder propertyIdParser propertyDecoder_


-- propertyDecoder_ : PropertyId -> D.Decoder Property
-- propertyDecoder_ id =
--     D.map3 Property
--         (D.succeed id)
--         (D.field "_rev" <| D.nullable D.string)
--         (D.field "version" D.int)


-- modificationDecoder : D.Decoder Modification
-- modificationDecoder =
--     parserDecoder modificationIdParser modificationDecoder_


-- modificationDecoder_ : ModificationId -> D.Decoder Modification
-- modificationDecoder_ id =
--     D.map5 Modification
--         (D.succeed id)
--         (D.field "_rev" D.string)
--         (D.field "version" D.int)
--         (D.field "docversion" D.int)
--         (D.field "docstate" D.value)


-- utilityDecoder : D.Decoder Utility
-- utilityDecoder =
--     parserDecoder utilityIdParser utilityDecoder_


-- utilityDecoder_ : UtilityId -> D.Decoder Utility
-- utilityDecoder_ id =
--     D.map4 Utility
--         (D.succeed id)
--         (D.field "_rev" D.string)
--         (D.field "version" D.int)
--         (D.field "value" D.value)


-- valueEncoder : Value -> E.Value
-- valueEncoder value =
--     case value of
--         MyPerson person ->
--             personEncoder person

--         MyTag tag ->
--             tagEncoder tag

--         MyProperty property ->
--             propertyEncoder property

--         MyUtility utility ->
--             utilityEncoder utility

--         MyModification modification ->
--             modificationEncoder modification

--         MyDescription description ->
--             descriptionEncoder description

--         MyInterlinear interlinear ->
--             interlinearEncoder interlinear


-- personEncoder : Person -> E.Value
-- personEncoder person =
--     E.object
--         [ ("_id", E.string ("person/" ++ (UUID.toString person.id)))
--         , ("_rev", E.string person.rev)
--         , ("version", E.int person.version)
--         , ("email", E.string person.email)
--         , ("names", E.dict identity E.int perons.names)
--         ]


-- tagEncoder : Tag -> E.Value
-- tagEncoder tag =
--     E.object
--         [ ("_id", E.string (tagIdBuilder tag))


-- identifierBuilder : List String -> List B.QueryParameter -> String -> String
-- identifierBuilder path query fragment =
--     (B.relative path query) ++ "#" ++ fragment
 
-- tagIdBuilder : Tag -> String
-- tagIdBuilder tag =
--     identifierBuilder
--     [ "tag", tag.kind ]
--     [ B.string "id" tag.docid ]
--     tag.fragment
