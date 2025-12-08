module Menkayonta exposing (..)

import Json.Decode as D
import Json.Decode.Extra as DE
import Json.Encode as E
import Json.Encode.Extra as EE
import Maybe.Extra as ME
import Dict exposing (Dict)
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
    , docid : String
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
    }
    

type alias Tag =
    { id : TagId
    , rev : Maybe String
    , version : Int
    }


type alias PropertyId =
    { kind : String
    , value : String
    , docid : String
    }
    

type alias Property =
    { id : PropertyId
    , rev : Maybe String
    , version : Int
    }


type alias Person =
    { id : UUID
    , rev : Maybe String
    , version : Int
    , email : String
    , names : Dict Int String
    }
    
    
type alias ModificationId =
    { kind : String
    , docid : String
    , time : Time.Posix
    , person : String
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
        

valueDecoder : D.Decoder Value
valueDecoder =
    D.field "_id" D.string
        |> D.andThen valueDecoder_


valueDecoder_ : String -> D.Decoder Value
valueDecoder_ id =
    case String.split "/" id of
        "interlinear" :: uuid :: [] ->
            case UUID.fromString uuid of
                Ok uuid_ ->
                    D.map MyInterlinear <| interlinearDecoder_ uuid_

                Err _ ->
                    D.fail "Invalid UUID in Interlinear ID"

        "person" :: uuid :: [] ->
            case UUID.fromString uuid of
                Ok uuid_ ->
                    D.map MyPerson <| personDecoder_ uuid_

                Err _ ->
                    D.fail "Invalid UUID in Person ID"

        "tag" :: kind :: docid ->
            String.join "/" docid
                |> TagId kind
                |> tagDecoder_
                |> D.map MyTag

        "description" :: kind :: docid ->
            String.join "/" docid
                |> DescriptionId kind
                |> descriptionDecoder_
                |> D.map MyDescription

        "utility" :: kind :: docid ->
            String.join "/" docid
                |> UtilityId kind
                |> utilityDecoder_
                |> D.map MyUtility
                
        "property" :: kind :: value :: docid ->
            String.join "/" docid
                |> PropertyId kind value
                |> propertyDecoder_
                |> D.map MyProperty

        "modification" :: kind :: d1 :: d2 :: time :: person :: [] ->
            
            D.map MyModification <|
                modificationDecoder_ { kind = kind
                                     , docid = d1 ++ "/" ++ d2
                                     , time = String.toInt time
                                     |> Maybe.withDefault 0
                                     |> Time.millisToPosix
                                     , person = person
                                     }

        _ ->
            D.fail "Unrecognized ID Type"
    

interlinearDecoder : D.Decoder Interlinear
interlinearDecoder =
    valueDecoder
        |> D.andThen (\i -> case i of
                                MyInterlinear i_ ->
                                    D.succeed i_

                                _ ->
                                    D.fail "Who could guess?"
                     )
       

interlinearDecoder_ : UUID -> D.Decoder Interlinear
interlinearDecoder_ id =
    D.map6 Interlinear
        (D.succeed id)
        (D.maybe <| D.field "_rev" D.string)
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
--     parserDecoder personIdParser Just personDecoder_
       

personDecoder_ : UUID -> D.Decoder Person
personDecoder_ id =
    D.map5 Person
        (D.succeed id)
        (D.maybe <| D.field "_rev" D.string)
        (D.field "version" D.int)
        (D.field "email" D.string)
        (D.field "names" (DE.dict2 D.int D.string))


-- descriptionDecoder : D.Decoder Description
-- descriptionDecoder =
--     let
--         converter id_ =
--             id_.docid
--                 |> Maybe.map (\d -> TagId id_.kind d id_.fragment)
--     in
--     parserDecoder descriptionIdParser converter descriptionDecoder_


descriptionDecoder_ : DescriptionId -> D.Decoder Description
descriptionDecoder_ id =
    D.map4 Description
        (D.succeed id)
        (D.field "_rev" <| D.nullable D.string)
        (D.field "version" D.int)
        (D.field "value" D.value)


-- tagDecoder : D.Decoder Tag
-- tagDecoder =
--     let
--         converter id_ =
--             id_.docid
--                 |> Maybe.map (\d -> TagId id_.kind d id_.fragment)
--     in
--     parserDecoder tagIdParser converter tagDecoder_


tagDecoder_ : TagId -> D.Decoder Tag
tagDecoder_ id =
    D.map3 Tag
        (D.succeed id)
        (D.maybe <| D.field "_rev" D.string)
        (D.field "version" D.int)

            
-- propertyDecoder : D.Decoder Property
-- propertyDecoder =
--     let
--         converter id_ =
--             id_.docid
--                 |> Maybe.map
--                    (\d -> PropertyId id_.kind id_.value d id_.fragment)
--     in
--     parserDecoder propertyIdParser converter propertyDecoder_


propertyDecoder_ : PropertyId -> D.Decoder Property
propertyDecoder_ id =
    D.map3 Property
        (D.succeed id)
        (D.field "_rev" <| D.nullable D.string)
        (D.field "version" D.int)


-- modificationDecoder : D.Decoder Modification
-- modificationDecoder =
--     let
--         converter m =
--             case (m.info.docid, m.info.time, m.info.personid) of
--                 (Just d, Just t, Just p) ->
--                     Just { kind = m.kind
--                          , docid = d
--                          , time = t
--                          , personid = p
--                          , fragment = m.fragment
--                          }

--                 _ ->
--                     Nothing
--     in
--     parserDecoder modificationIdParser converter modificationDecoder_


modificationDecoder_ : ModificationId -> D.Decoder Modification
modificationDecoder_ id =
    D.map5 Modification
        (D.succeed id)
        (D.field "_rev" D.string)
        (D.field "version" D.int)
        (D.field "docversion" D.int)
        (D.field "docstate" D.value)


-- utilityDecoder : D.Decoder Utility
-- utilityDecoder =
--     let
--         converter id_ =
--             id_.docid |> Maybe.map (UtilityId id_.kind)
--     in
--     parserDecoder utilityIdParser converter utilityDecoder_


utilityDecoder_ : UtilityId -> D.Decoder Utility
utilityDecoder_ id =
    D.map4 Utility
        (D.succeed id)
        (D.field "_rev" D.string)
        (D.field "version" D.int)
        (D.field "value" D.value)


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
