module Menkayonta exposing
    ( Description
    , DescriptionId
    , Glosses
    , Interlinear
    , Modification
    , ModificationId
    , Property
    , PropertyId
    , Tag
    , TagId
    , Translation
    , Utility
    , UtilityId
    , Value(..)
    , decoder
    )

import Dict exposing (Dict)
import Json.Decode as D
import Json.Decode.Extra as DE
import Json.Encode as E
import Json.Encode.Extra as EE
import Maybe.Extra as ME
import SharedTypes as ST
import Time
import UUID exposing (UUID)


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


type alias Glosses =
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
    , glosses : Glosses
    , translations : Dict Int Translation
    }


type alias DescriptionId =
    { kind : String
    , docid : Identifier
    , fragment : List String
    }


type alias Description =
    { id : DescriptionId
    , rev : Maybe String
    , version : Int
    , value : E.Value
    }


type alias TagId =
    { kind : String
    , docid : Identifier
    , fragment : List String
    }


type alias Tag =
    { id : TagId
    , rev : Maybe String
    , version : Int
    }


type alias PropertyId =
    { kind : String
    , value : String
    , docid : Identifier
    , fragment : List String
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
    , docid : Identifier
    , time : Time.Posix
    , person : Identifier
    , fragment : List String
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
    , docid : Identifier
    , fragment : List String
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


decoder : D.Decoder Value
decoder =
    D.field "_id" D.string
        |> D.andThen decoder_


decoder_ : String -> D.Decoder Value
decoder_ idstr =
    let
        id =
            stringToIdentifier idstr
    in
    case id of
        Just (MyInterlinearId id_) ->
            D.map MyInterlinear <| interlinearDecoder_ id_

        Just (MyPersonId id_) ->
            D.map MyPerson <| personDecoder_ id_

        Just (MyTagId id_) ->
            D.map MyTag <| tagDecoder_ id_

        Just (MyDescriptionId id_) ->
            D.map MyDescription <| descriptionDecoder_ id_

        Just (MyUtilityId id_) ->
            D.map MyUtility <| utilityDecoder_ id_

        Just (MyPropertyId id_) ->
            D.map MyProperty <| propertyDecoder_ id_

        Just (MyModificationId id_) ->
            D.map MyModification <| modificationDecoder_ id_

        Nothing ->
            D.fail "Unrecognized Document Type"


documentIdentifier : ( String, String ) -> Maybe Identifier
documentIdentifier idpair =
    case idpair of
        ( "interlinear", uuidstr ) ->
            UUID.fromString uuidstr
                |> Result.toMaybe
                |> Maybe.map MyInterlinearId

        ( "person", uuidstr ) ->
            UUID.fromString uuidstr
                |> Result.toMaybe
                |> Maybe.map MyPersonId

        _ ->
            Nothing


stringToIdentifier : String -> Maybe Identifier
stringToIdentifier idstring =
    let
        idlists =
            case String.split "#" idstring of
                one :: two :: [] ->
                    ( String.split "/" one, String.split "/" two )

                hd :: [] ->
                    ( String.split "/" hd, [] )

                _ ->
                    -- This is a bad id, it will be caught below.
                    ( [], [] )
    in
    case idlists of
        ( d1 :: d2 :: [], [] ) ->
            documentIdentifier ( d1, d2 )

        ( "tag" :: kind :: d1 :: d2 :: [], fragment ) ->
            documentIdentifier ( d1, d2 )
                |> Maybe.map (\d -> TagId kind d fragment)
                |> Maybe.map MyTagId

        ( "description" :: kind :: d1 :: d2 :: [], fragment ) ->
            documentIdentifier ( d1, d2 )
                |> Maybe.map (\d -> DescriptionId kind d fragment)
                |> Maybe.map MyDescriptionId

        ( "utility" :: kind :: d1 :: d2 :: [], fragment ) ->
            documentIdentifier ( d1, d2 )
                |> Maybe.map (\d -> UtilityId kind d fragment)
                |> Maybe.map MyUtilityId

        ( "property" :: kind :: value :: d1 :: d2 :: [], fragment ) ->
            documentIdentifier ( d1, d2 )
                |> Maybe.map (\d -> PropertyId kind value d fragment)
                |> Maybe.map MyPropertyId

        ( "modification" :: kind :: d1 :: d2 :: time :: person :: [], fragment ) ->
            documentIdentifier ( d1, d2 )
                |> Maybe.andThen
                    (\d ->
                        documentIdentifier ( "person", person )
                            |> Maybe.andThen
                                (\p ->
                                    String.toInt time
                                        |> Maybe.map Time.millisToPosix
                                        |> Maybe.map
                                            (\t -> { kinde = kind
                                                   , docid = d
                                                   , time = t
                                                   , person = p
                                                   , fragment = fragment
                                                   }
                                            )
                                )
                    )
                |> Maybe.map MyModificationId

        _ ->
            Nothing


interlinearDecoder_ : UUID -> D.Decoder Interlinear
interlinearDecoder_ id =
    D.map6 Interlinear
        (D.succeed id)
        (D.maybe <| D.field "_rev" D.string)
        (D.field "version" D.int)
        (D.field "text" D.string)
        (D.field "glosses" glossesDecoder)
        (D.field "translations" (DE.dict2 D.int translationDecoder))


glossesDecoder : D.Decoder Glosses
glossesDecoder =
    D.map4 Glosses
        (D.field "breaks" D.string)
        (D.field "glosses" D.string)
        (D.field "phonemic" D.string)
        (D.field "judgment" D.string)


translationDecoder : D.Decoder Translation
translationDecoder =
    D.map2 Translation
        (D.field "translation" D.string)
        (D.field "judgment" D.string)


personDecoder_ : UUID -> D.Decoder Person
personDecoder_ id =
    D.map5 Person
        (D.succeed id)
        (D.maybe <| D.field "_rev" D.string)
        (D.field "version" D.int)
        (D.field "email" D.string)
        (D.field "names" (DE.dict2 D.int D.string))


descriptionDecoder_ : DescriptionId -> D.Decoder Description
descriptionDecoder_ id =
    D.map4 Description
        (D.succeed id)
        (D.field "_rev" <| D.nullable D.string)
        (D.field "version" D.int)
        (D.field "value" D.value)


tagDecoder_ : TagId -> D.Decoder Tag
tagDecoder_ id =
    D.map3 Tag
        (D.succeed id)
        (D.maybe <| D.field "_rev" D.string)
        (D.field "version" D.int)


propertyDecoder_ : PropertyId -> D.Decoder Property
propertyDecoder_ id =
    D.map3 Property
        (D.succeed id)
        (D.field "_rev" <| D.nullable D.string)
        (D.field "version" D.int)


modificationDecoder_ : ModificationId -> D.Decoder Modification
modificationDecoder_ id =
    D.map5 Modification
        (D.succeed id)
        (D.field "_rev" D.string)
        (D.field "version" D.int)
        (D.field "docversion" D.int)
        (D.field "docstate" D.value)


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
