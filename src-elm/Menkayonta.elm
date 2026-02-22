module Menkayonta exposing
    ( Annotations
    , Description
    , DescriptionId
    , DocId(..)
    , Identifier(..)
    , Interlinear
    , Modification
    , ModificationId
    , Composite
    , Person
    , Property
    , PropertyId
    , Tag
    , TagId
    , Translation
    , Utility
    , UtilityId
    , Value(..)
    , encoder
    , identifierToReverse
    , identifierToString
    , listDecoder
    , compositeBuilder
    , stringToIdentifier
    )

import Dict exposing (Dict)
import Email
import Json.Decode as D
import Json.Decode.Extra as DE
import Json.Encode as E
import Time
import UUID exposing (UUID)
import Url
import Url.Builder exposing (Root(..))
import Url.Parser as UP exposing ((</>))


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
type alias Composite =
    { doc : Maybe Value
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
    , fragment : Maybe String
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
    , fragment : Maybe String
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
    , fragment : Maybe String
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
    , fragment : Maybe String
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
    , fragment : Maybe String
    }


type alias Utility =
    { id : UtilityId
    , rev : Maybe String
    , version : Int
    , value : E.Value
    }


idParser : UP.Parser (Identifier -> a) a
idParser =
    let
        interlinearId : UP.Parser (UUID.UUID -> b) b
        interlinearId =
            UP.custom "UUID"
                (\s ->
                    UUID.fromString s
                        |> Result.toMaybe
                )

        personId : UP.Parser (String -> b) b
        personId =
            UP.custom "EMAIL"
                (\s ->
                    Url.percentDecode s
                        |> Maybe.andThen Email.fromString
                        |> Maybe.map Email.toString
                )

        urlTime : UP.Parser (Time.Posix -> b) b
        urlTime =
            UP.custom "TIME.POSIX"
                (\s ->
                    String.toInt s
                        |> Maybe.map Time.millisToPosix
                )

        docId : UP.Parser (DocId -> b) b
        docId =
            UP.oneOf
                [ UP.map InterlinearId
                    (UP.s "interlinear" </> interlinearId)
                , UP.map PersonId
                    (UP.s "person" </> personId)
                ]

        urlDecode : String -> String
        urlDecode s =
            Url.percentDecode s
                |> Maybe.withDefault s

        tagId : DocId -> String -> Maybe String -> Identifier
        tagId docid kind fragment =
            { kind = urlDecode kind
            , docid = docid
            , fragment = fragment
            }
                |> MyTagId

        descriptionId : DocId -> String -> Maybe String -> Identifier
        descriptionId docid kind fragment =
            { kind = urlDecode kind
            , docid = docid
            , fragment = fragment
            }
                |> MyDescriptionId

        propertyId :
            DocId
            -> String
            -> String
            -> Maybe String
            -> Identifier
        propertyId docid kind value fragment =
            { kind = urlDecode kind
            , value = urlDecode value
            , docid = docid
            , fragment = fragment
            }
                |> MyPropertyId

        utilityId : String -> DocId -> Maybe String -> Identifier
        utilityId kind docid fragment =
            { kind = urlDecode kind
            , docid = docid
            , fragment = fragment
            }
                |> MyUtilityId

        modificationId :
            DocId
            -> String
            -> Time.Posix
            -> String
            -> Maybe String
            -> Identifier
        modificationId docid kind time person fragment =
            { kind = urlDecode kind
            , docid = docid
            , time = time
            , person = PersonId person
            , fragment = fragment
            }
                |> MyModificationId
    in
    UP.oneOf
        [ UP.map MyDocId docId
        , UP.map tagId
            (docId
                </> UP.s "tag"
                </> UP.string
                </> UP.fragment identity
            )
        , UP.map descriptionId
            (docId
                </> UP.s "description"
                </> UP.string
                </> UP.fragment identity
            )
        , UP.map utilityId
            (UP.s "utility"
                </> UP.string
                </> docId
                </> UP.fragment identity
            )
        , UP.map propertyId
            (docId
                </> UP.s "property"
                </> UP.string
                </> UP.string
                </> UP.fragment identity
            )
        , UP.map modificationId
            (docId
                </> UP.s "modification"
                </> UP.string
                </> urlTime
                </> personId
                </> UP.fragment identity
            )
        ]


stringToIdentifier : String -> Maybe Identifier
stringToIdentifier s =
    Url.fromString ("http://example.com/" ++ s)
        |> Maybe.andThen (UP.parse idParser)


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


docIdToList : DocId -> List String
docIdToList docid =
    case docid of
        PersonId id ->
            [ "person", id ]
                |> List.map Url.percentEncode

        InterlinearId uuid ->
            [ "interlinear", UUID.toString uuid ]
                |> List.map Url.percentEncode


docIdToString : DocId -> String
docIdToString docid =
    Url.Builder.relative (docIdToList docid) []


{-| Unlike docIdToString, this returns only the string representation
of the id part, rather than a URL path-like object.
-}
docIdToSimpleString : DocId -> String
docIdToSimpleString docid =
    case docid of
        PersonId id ->
            id

        InterlinearId uuid ->
            UUID.toString uuid


descriptionIdToString : DescriptionId -> String
descriptionIdToString descriptionid =
    let
        path : List String
        path =
            docIdToList descriptionid.docid
                ++ [ "description"
                   , descriptionid.kind
                   ]
                |> List.map Url.percentEncode
    in
    Url.Builder.custom Relative path [] descriptionid.fragment


modificationIdToString : ModificationId -> String
modificationIdToString modid =
    let
        path : List String
        path =
            docIdToList modid.docid
                ++ [ "modification"
                   , modid.kind
                   , Time.posixToMillis modid.time |> String.fromInt
                   , docIdToSimpleString modid.person
                   ]
                |> List.map Url.percentEncode
    in
    Url.Builder.custom Relative path [] modid.fragment


tagIdToString : TagId -> String
tagIdToString tagid =
    let
        path : List String
        path =
            docIdToList tagid.docid
                ++ [ "tag"
                   , tagid.kind
                   ]
                |> List.map Url.percentEncode
    in
    Url.Builder.custom Relative path [] tagid.fragment


propertyIdToString : PropertyId -> String
propertyIdToString propertyid =
    let
        path : List String
        path =
            docIdToList propertyid.docid
                ++ [ "property"
                   , propertyid.kind
                   , propertyid.value
                   ]
                |> List.map Url.percentEncode
    in
    Url.Builder.custom Relative path [] propertyid.fragment


utilityIdToString : UtilityId -> String
utilityIdToString utilityid =
    let
        path : List String
        path =
            "utility"
                :: utilityid.kind
                :: docIdToList utilityid.docid
                |> List.map Url.percentEncode
    in
    Url.Builder.custom Relative path [] utilityid.fragment


{-| Provide a URL fragment that can be used to lookup documents by
their metadata.
-}
identifierToReverse : Identifier -> Maybe String
identifierToReverse ident =
    case ident of
        MyDocId _ ->
            Nothing

        MyTagId ident_ ->
            Just <| tagIdToReverse ident_

        MyPropertyId ident_ ->
            Just <| propertyIdToReverse ident_

        MyDescriptionId ident_ ->
            Just <| descriptionIdToReverse ident_

        MyModificationId ident_ ->
            Just <| modificationIdToReverse ident_

        MyUtilityId _ ->
            Nothing


descriptionIdToReverse : DescriptionId -> String
descriptionIdToReverse descriptionid =
    let
        path : List String
        path =
            [ "description"
            , descriptionid.kind
            ]
             |> List.map Url.percentEncode
    in
    Url.Builder.custom Relative path [] Nothing


modificationIdToReverse : ModificationId -> String
modificationIdToReverse modid =
    let
        path : List String
        path =
            [ "modification"
            , modid.kind
            ]
             |> List.map Url.percentEncode
    in
    Url.Builder.custom Relative path [] Nothing


tagIdToReverse : TagId -> String
tagIdToReverse tagid =
    let
        path : List String
        path =
            [ "tag"
            , tagid.kind
            ]
             |> List.map Url.percentEncode
    in
    Url.Builder.custom Relative path [] Nothing


propertyIdToReverse : PropertyId -> String
propertyIdToReverse propertyid =
    let
        path : List String
        path =
            [ "property"
            , propertyid.kind
            , propertyid.value
            ]
             |> List.map Url.percentEncode
    in
    Url.Builder.custom Relative path [] Nothing



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
        id : Maybe Identifier
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



{- Utility functions -}


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


{-| A helper function to use in folds.
-}
compositeBuilder : Value -> Composite -> Composite
compositeBuilder v od =
    case v of
        MyTag t ->
            { od | tags = t :: od.tags }

        MyProperty p ->
            { od | properties = p :: od.properties }

        MyDescription d ->
            { od | descriptions = d :: od.descriptions }

        MyModification m ->
            { od | modifications = m :: od.modifications }

        MyUtility _ ->
            od

        other ->
            { od | doc = Just other }
