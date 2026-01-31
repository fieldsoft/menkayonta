module Menkayonta exposing
    ( Annotations
    , Description
    , DescriptionId
    , DocId(..)
    , Identifier(..)
    , Interlinear
    , Modification
    , ModificationId
    , OneDoc
    , Person
    , Property
    , PropertyId
    , Tag
    , TagId
    , Translation
    , Utility
    , UtilityId
    , Value(..)
    , decoder
    , encoder
    , identifierToString
    , listDecoder
    , people
    , stringToIdentifier
    )

import Dict exposing (Dict, values)
import Email exposing (Email)
import Json.Decode as D
import Json.Decode.Extra as DE
import Json.Encode as E
import Maybe.Extra as ME
import Time
import UUID exposing (UUID)


{-| These are the basic kinds of values that are used to represent
application data in Menkayonta.
-}
type Value
    = MyPerson Person
    | MyTag Tag
    | MyProperty Property
    | MyUtility Utility
    | MyModification Modification
    | MyDescription Description
    | MyInterlinear Interlinear


{-| Identifers for data types are complex data types, themselves.
-}
type Identifier
    = MyDocId DocId
    | MyTagId TagId
    | MyDescriptionId DescriptionId
    | MyPropertyId PropertyId
    | MyUtilityId UtilityId
    | MyModificationId ModificationId


{-| Most identifiers are identifiers for metadata. These are for
documents, data structures that contain non-metadata.
-}
type DocId
    = PersonId String
    | InterlinearId UUID


{-| When working with documents in forms and other contexts, it is
useful to have a single document with the core data and all
metadata.
-}
type alias OneDoc =
    { doc : Maybe Value
    , tags : List Tag
    , properties : List Property
    , descriptions : List Description
    , modifications : List Modification
    }


type alias OnePersonDoc =
    { person : Person
    , tags : List Tag
    , properties : List Property
    , descriptions : List Description
    , modifications : List Modification
    }


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
    , ann : Annotations
    , translations : Dict Int Translation
    }


type alias DescriptionId =
    { kind : String
    , docid : DocId
    , fragment : List String
    }


type alias Description =
    { id : DescriptionId
    , rev : Maybe String
    , version : Int
    , value : String
    }


type alias TagId =
    { kind : String
    , docid : DocId
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
    , docid : DocId
    , fragment : List String
    }


type alias Property =
    { id : PropertyId
    , rev : Maybe String
    , version : Int
    }


type alias Person =
    { id : String
    , rev : Maybe String
    , version : Int
    , names : Dict Int String
    }


type alias ModificationId =
    { kind : String
    , docid : DocId
    , time : Time.Posix
    , person : DocId
    , fragment : List String
    }


type alias Modification =
    { id : ModificationId
    , rev : Maybe String
    , version : Int
    , comment : String
    , docversion : Int
    , value : E.Value
    }


type alias UtilityId =
    { kind : String
    , docid : DocId
    , fragment : List String
    }


type alias Utility =
    { id : UtilityId
    , rev : Maybe String
    , version : Int
    , value : E.Value
    }


documentIdentifier : ( String, String ) -> Maybe DocId
documentIdentifier idpair =
    case idpair of
        ( "interlinear", uuidstr ) ->
            UUID.fromString uuidstr
                |> Result.toMaybe
                |> Maybe.map InterlinearId

        ( "person", id ) ->
            Email.fromString id
                |> Maybe.map Email.toString
                |> Maybe.map PersonId

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
                    -- This case is a bad id, it will be caught in the
                    -- main body of the function.
                    ( [], [] )
    in
    case idlists of
        ( d1 :: d2 :: [], [] ) ->
            documentIdentifier ( d1, d2 ) |> Maybe.map MyDocId

        ( d1 :: d2 :: "tag" :: kind :: [], fragment ) ->
            documentIdentifier ( d1, d2 )
                |> Maybe.map (\d -> TagId kind d fragment)
                |> Maybe.map MyTagId

        ( d1 :: d2 :: "description" :: kind :: [], fragment ) ->
            documentIdentifier ( d1, d2 )
                |> Maybe.map (\d -> DescriptionId kind d fragment)
                |> Maybe.map MyDescriptionId

        ( "utility" :: kind :: d1 :: d2 :: [], fragment ) ->
            documentIdentifier ( d1, d2 )
                |> Maybe.map (\d -> UtilityId kind d fragment)
                |> Maybe.map MyUtilityId

        ( d1 :: d2 :: "property" :: kind :: value :: [], fragment ) ->
            documentIdentifier ( d1, d2 )
                |> Maybe.map (\d -> PropertyId kind value d fragment)
                |> Maybe.map MyPropertyId

        ( d1 :: d2 :: "modification" :: kind :: time :: person :: [], fragment ) ->
            documentIdentifier ( d1, d2 )
                |> Maybe.andThen
                    (\d ->
                        documentIdentifier ( "person", person )
                            |> Maybe.andThen
                                (\p ->
                                    String.toInt time
                                        |> Maybe.map Time.millisToPosix
                                        |> Maybe.map
                                            (\t ->
                                                { kind = kind
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



{- Functions for converting Identifiers to Strings -}


{-| Convert and Identifier to a String.
-}
identifierToString : Identifier -> String
identifierToString ident =
    case ident of
        MyDocId ident_ ->
            docIdToString ident_

        MyTagId ident_ ->
            tagIdToString ident_

        MyPropertyId ident_ ->
            propertyIdToString ident_

        MyDescriptionId ident_ ->
            descriptionIdToString ident_

        MyModificationId ident_ ->
            modificationIdToString ident_

        MyUtilityId ident_ ->
            utilityIdToString ident_


docIdToString : DocId -> String
docIdToString docid =
    let
        appendUUID doctype uuid =
            doctype ++ "/" ++ UUID.toString uuid
    in
    case docid of
        PersonId id ->
            "person" ++ "/" ++ id

        InterlinearId uuid ->
            appendUUID "interlinear" uuid


{-| Unlike docIdToString, this returns only the string representation
of the UUID part.
-}
docUuidToString : DocId -> String
docUuidToString docid =
    case docid of
        PersonId id ->
            id

        InterlinearId uuid ->
            UUID.toString uuid


descriptionIdToString : DescriptionId -> String
descriptionIdToString descriptionid =
    [ docIdToString descriptionid.docid
    , "description"
    , descriptionid.kind
    ]
        |> addFrag descriptionid.fragment


modificationIdToString : ModificationId -> String
modificationIdToString modid =
    [ docIdToString modid.docid
    , "modification"
    , modid.kind
    , Time.posixToMillis modid.time |> String.fromInt
    , docUuidToString modid.person
    ]
        |> addFrag modid.fragment


tagIdToString : TagId -> String
tagIdToString tagid =
    [ docIdToString tagid.docid
    , "tag"
    , tagid.kind
    ]
        |> addFrag tagid.fragment


propertyIdToString : PropertyId -> String
propertyIdToString propertyid =
    [ docIdToString propertyid.docid
    , "property"
    , propertyid.kind
    , propertyid.value
    ]
        |> addFrag propertyid.fragment


utilityIdToString : UtilityId -> String
utilityIdToString utilityid =
    [ "utility"
    , utilityid.kind
    , docIdToString utilityid.docid
    ]
        |> addFrag utilityid.fragment



{- JSON Encoder/Decoder functions -}


listDecoder : D.Decoder (List Value)
listDecoder =
    D.list decoder


{-| Encode any Menkayonta Value to a JSON Value
-}
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
        Just (MyDocId (InterlinearId id_)) ->
            D.map MyInterlinear <| interlinearDecoder_ id_

        Just (MyDocId (PersonId id_)) ->
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


interlinearDecoder_ : UUID -> D.Decoder Interlinear
interlinearDecoder_ id =
    D.map6 Interlinear
        (D.succeed id)
        (D.maybe <| D.field "_rev" D.string)
        (D.field "version" D.int)
        (D.field "text" D.string)
        (D.field "ann" annDecoder)
        (D.field "translations" (DE.dict2 D.int translationDecoder))


annDecoder : D.Decoder Annotations
annDecoder =
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


personDecoder_ : String -> D.Decoder Person
personDecoder_ id =
    D.map4 Person
        (D.succeed id)
        (D.maybe <| D.field "_rev" D.string)
        (D.field "version" D.int)
        (D.field "names" (DE.dict2 D.int D.string))


descriptionDecoder_ : DescriptionId -> D.Decoder Description
descriptionDecoder_ id =
    D.map4 Description
        (D.succeed id)
        (D.field "_rev" <| D.nullable D.string)
        (D.field "version" D.int)
        (D.field "value" D.string)


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
    D.map6 Modification
        (D.succeed id)
        (D.maybe <| D.field "_rev" D.string)
        (D.field "version" D.int)
        (D.field "comment" D.string)
        (D.field "docversion" D.int)
        (D.field "value" D.value)


utilityDecoder_ : UtilityId -> D.Decoder Utility
utilityDecoder_ id =
    D.map4 Utility
        (D.succeed id)
        (D.maybe <| D.field "_rev" D.string)
        (D.field "version" D.int)
        (D.field "value" D.value)



{- JSON Encoder functions -}


{-| Encode any Menkayonta Value to a JSON Value
-}
encoder : Value -> E.Value
encoder value =
    case value of
        MyPerson person ->
            personEncoder person

        MyInterlinear interlinear ->
            interlinearEncoder interlinear

        MyTag tag ->
            tagEncoder tag

        MyProperty property ->
            propertyEncoder property

        MyUtility utility ->
            utilityEncoder utility

        MyModification modification ->
            modificationEncoder modification

        MyDescription description ->
            descriptionEncoder description


{-| People may be updated. The revision on encoding is not always
abscent, since writing the document to the database may result in an
update.
-}
personEncoder : Person -> E.Value
personEncoder person =
    [ ( "_id", E.string (docIdToString <| PersonId person.id) )
    , ( "version", E.int person.version )
    , ( "names", E.dict String.fromInt E.string person.names )
    ]
        |> addRev person.rev


{-| Interlinears may be updated. The revision on encoding is not
always abscent, since writing the document to the database may result
in an update.
-}
interlinearEncoder : Interlinear -> E.Value
interlinearEncoder int =
    [ ( "_id", E.string (docIdToString <| InterlinearId int.id) )
    , ( "version", E.int int.version )
    , ( "text", E.string int.text )
    , ( "ann", annEncoder int.ann )
    , ( "translations", E.dict String.fromInt translationEncoder int.translations )
    ]
        |> addRev int.rev


annEncoder : Annotations -> E.Value
annEncoder ann =
    E.object
        [ ( "breaks", E.string ann.breaks )
        , ( "glosses", E.string ann.glosses )
        , ( "phonemic", E.string ann.phonemic )
        , ( "judgment", E.string ann.judgment )
        ]


translationEncoder : Translation -> E.Value
translationEncoder tr =
    E.object
        [ ( "translation", E.string tr.translation )
        , ( "judgment", E.string tr.judgment )
        ]


tagEncoder : Tag -> E.Value
tagEncoder tag =
    [ ( "_id", E.string (tagIdToString tag.id) )
    , ( "version", E.int tag.version )
    ]
        |> addRev tag.rev


propertyEncoder : Property -> E.Value
propertyEncoder property =
    [ ( "_id", E.string (propertyIdToString property.id) )
    , ( "version", E.int property.version )
    ]
        |> addRev property.rev


utilityEncoder : Utility -> E.Value
utilityEncoder utility =
    [ ( "_id", E.string (utilityIdToString utility.id) )
    , ( "version", E.int utility.version )
    , ( "value", utility.value )
    ]
        |> addRev utility.rev


descriptionEncoder : Description -> E.Value
descriptionEncoder description =
    [ ( "_id", E.string (descriptionIdToString description.id) )
    , ( "version", E.int description.version )
    , ( "value", E.string description.value )
    ]
        |> addRev description.rev


modificationEncoder : Modification -> E.Value
modificationEncoder modification =
    [ ( "_id", E.string (modificationIdToString modification.id) )
    , ( "version", E.int modification.version )
    , ( "comment", E.string modification.comment )
    , ( "docversion", E.int modification.docversion )
    , ( "value", modification.value )
    ]
        |> addRev modification.rev


people : List Value -> List Person
people vals =
    let
        maybePerson val =
            case val of
                MyPerson p ->
                    Just p

                _ ->
                    Nothing
    in
    List.map maybePerson vals
        |> ME.values



{- Utility functions -}


addIds : List String -> Maybe String -> List ( String, E.Value ) -> List E.Value
addIds ids rev vs =
    List.map (\id -> ( "_id", E.string id ) :: vs |> addRev rev) ids


{-| The function addRev adds a field to a JSON Value, rather than
providing a key with a null value, the field is either there or not
there.
-}
addRev : Maybe String -> List ( String, E.Value ) -> E.Value
addRev rev fields =
    rev
        |> Maybe.map (\rev_ -> ( "_rev", E.string rev_ ))
        |> Maybe.map (\rev_ -> rev_ :: fields)
        |> Maybe.map E.object
        |> Maybe.withDefault (E.object fields)


{-| The function addFrag ensures that there are no empty fragments following a '#'.
-}
addFrag : List String -> List String -> String
addFrag s1 s2 =
    let
        s1_ =
            String.join "/" s1
    in
    if s1_ == "" then
        String.join "/" s2

    else
        String.join "/" s2 ++ "#" ++ s1_
