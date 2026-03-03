module Menkayonta exposing
    ( Annotations
    , Composite
    , Description
    , DescriptionId
    , DocId(..)
    , Identifier(..)
    , Interlinear
    , Link
    , LinkId
    , Modification
    , ModificationId
    , Note
    , NoteId
    , Page
    , Person
    , Property
    , PropertyId
    , Sequence
    , Tag
    , TagId
    , Translation
    , Utility
    , UtilityId
    , Value(..)
    , compositeBuilder
    , encoder
    , identifierToReverse
    , identifierToString
    , listDecoder
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
    | MyInterlinear Interlinear
    | MySequence Sequence
    | MyPage Page
    | MyNote Note
    | MyTag Tag
    | MyProperty Property
    | MyUtility Utility
    | MyModification Modification
    | MyDescription Description
    | MyLink Link


{-| Identifers for data types are complex data types, themselves.
-}
type Identifier
    = MyDocId DocId
    | MyTagId TagId
    | MyDescriptionId DescriptionId
    | MyPropertyId PropertyId
    | MyUtilityId UtilityId
    | MyModificationId ModificationId
    | MyLinkId LinkId
    | MyNoteId NoteId


{-| Most identifiers are identifiers for metadata. These are for
documents, data structures that contain non-metadata.
-}
type DocId
    = PersonId String
    | InterlinearId UUID
    | SequenceId UUID
    | PageId UUID


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
    , links : List Link
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


type alias Page =
    { id : UUID
    , rev : Maybe String
    , version : Int
    , title : String
    , description : String
    }


type alias Sequence =
    { id : UUID
    , rev : Maybe String
    , version : Int
    , kind : String
    , title : String
    , description : String
    , items : List { key : E.Value, value : UUID.UUID }
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


type alias LinkId =
    { kind : String
    , fromid : DocId
    , toid : DocId
    , fragment : Maybe String
    }


type alias Link =
    { id : LinkId
    , rev : Maybe String
    , version : Int
    }


type alias NoteId =
    DocId


type alias Note =
    { id : NoteId
    , rev : Maybe String
    , version : Int
    , note : String
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


stringToIdentifier : String -> Maybe Identifier
stringToIdentifier s =
    Url.fromString ("http://example.com/" ++ s)
        |> Maybe.andThen (UP.parse idParser)


idParser : UP.Parser (Identifier -> a) a
idParser =
    let
        uuid : UP.Parser (UUID.UUID -> b) b
        uuid =
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
                    (UP.s "interlinear" </> uuid)
                , UP.map SequenceId
                    (UP.s "sequence" </> uuid)
                , UP.map PageId
                    (UP.s "page" </> uuid)
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

        linkId :
            DocId
            -> String
            -> DocId
            -> Maybe String
            -> Identifier
        linkId fromid kind toid fragment =
            { kind = urlDecode kind
            , fromid = fromid
            , toid = toid
            , fragment = fragment
            }
                |> MyLinkId
    in
    UP.oneOf
        [ UP.map MyDocId docId
        , UP.map tagId
            (docId
                </> UP.s "tag"
                </> UP.string
                </> UP.fragment identity
            )
        , UP.map MyNoteId
            (docId </> UP.s "note")
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
        , UP.map linkId
            (docId
                </> UP.s "link"
                </> UP.string
                </> docId
                </> UP.fragment identity
            )
        ]


{-| Convert and Identifier to a String.
-}
identifierToString : Identifier -> String
identifierToString ident =
    case ident of
        MyDocId ident_ ->
            docIdToString ident_

        MyNoteId ident_ ->
            noteIdToString ident_

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

        MyLinkId ident_ ->
            linkIdToString ident_


docIdToList : DocId -> List String
docIdToList docid =
    case docid of
        PersonId id ->
            [ "person", id ]
                |> List.map Url.percentEncode

        InterlinearId uuid ->
            [ "interlinear", UUID.toString uuid ]
                |> List.map Url.percentEncode

        SequenceId uuid ->
            [ "sequence", UUID.toString uuid ]
                |> List.map Url.percentEncode

        PageId uuid ->
            [ "page", UUID.toString uuid ]
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

        SequenceId uuid ->
            UUID.toString uuid

        PageId uuid ->
            UUID.toString uuid


descriptionIdToString : DescriptionId -> String
descriptionIdToString descriptionid =
    let
        path : List String
        path =
            [ "description", descriptionid.kind ]
                |> List.map Url.percentEncode
                |> List.append (docIdToList descriptionid.docid)
    in
    Url.Builder.custom Relative path [] descriptionid.fragment


modificationIdToString : ModificationId -> String
modificationIdToString modid =
    let
        path : List String
        path =
            [ "modification"
            , modid.kind
            , Time.posixToMillis modid.time |> String.fromInt
            , docIdToSimpleString modid.person
            ]
                |> List.map Url.percentEncode
                |> List.append (docIdToList modid.docid)
    in
    Url.Builder.custom Relative path [] modid.fragment


linkIdToString : LinkId -> String
linkIdToString linkid =
    let
        path : List String
        path =
            docIdToList linkid.toid
                |> List.append
                    ([ "link", linkid.kind ]
                        |> List.map Url.percentEncode
                    )
                |> List.append (docIdToList linkid.fromid)
    in
    Url.Builder.custom Relative path [] linkid.fragment


noteIdToString : NoteId -> String
noteIdToString noteid =
    let
        path : List String
        path =
            [ "note " ]
                |> List.append (docIdToList noteid)
    in
    Url.Builder.custom Relative path [] Nothing


tagIdToString : TagId -> String
tagIdToString tagid =
    let
        path : List String
        path =
            [ "tag", tagid.kind ]
                |> List.map Url.percentEncode
                |> List.append (docIdToList tagid.docid)
    in
    Url.Builder.custom Relative path [] tagid.fragment


propertyIdToString : PropertyId -> String
propertyIdToString propertyid =
    let
        path : List String
        path =
            [ "property", propertyid.kind, propertyid.value ]
                |> List.map Url.percentEncode
                |> List.append (docIdToList propertyid.docid)
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

        MyLinkId ident_ ->
            Just <| linkIdToReverse ident_

        MyNoteId _ ->
            Just <| noteIdToReverse

        MyUtilityId _ ->
            Nothing


noteIdToReverse : String
noteIdToReverse =
    Url.Builder.custom Relative [ "note" ] [] Nothing


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


linkIdToReverse : LinkId -> String
linkIdToReverse linkid =
    let
        path : List String
        path =
            [ "link"
            , linkid.kind
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
            D.map MyInterlinear <| interlinearDecoder id_

        Just (MyDocId (SequenceId id_)) ->
            D.map MySequence <| sequenceDecoder id_

        Just (MyDocId (PageId id_)) ->
            D.map MyPage <| pageDecoder id_

        Just (MyDocId (PersonId id_)) ->
            D.map MyPerson <| personDecoder id_

        Just (MyNoteId id_) ->
            D.map MyNote <| noteDecoder id_

        Just (MyTagId id_) ->
            D.map MyTag <| tagDecoder id_

        Just (MyDescriptionId id_) ->
            D.map MyDescription <| descriptionDecoder id_

        Just (MyUtilityId id_) ->
            D.map MyUtility <| utilityDecoder id_

        Just (MyPropertyId id_) ->
            D.map MyProperty <| propertyDecoder id_

        Just (MyModificationId id_) ->
            D.map MyModification <| modificationDecoder id_

        Just (MyLinkId id_) ->
            D.map MyLink <| linkDecoder id_

        Nothing ->
            D.fail "Unrecognized Document Type"


interlinearDecoder : UUID -> D.Decoder Interlinear
interlinearDecoder id =
    D.map6 Interlinear
        (D.succeed id)
        (D.maybe <| D.field "_rev" D.string)
        (D.field "version" D.int)
        (D.field "text" D.string)
        (D.field "ann" annDecoder)
        (D.field "translations" (DE.dict2 D.int translationDecoder))


pageDecoder : UUID -> D.Decoder Page
pageDecoder id =
    D.map5 Page
        (D.succeed id)
        (D.maybe <| D.field "_rev" D.string)
        (D.field "version" D.int)
        (D.field "title" D.string)
        (D.field "description" D.string)


sequenceDecoder : UUID -> D.Decoder Sequence
sequenceDecoder id =
    let
        itemDecoder : D.Decoder { key : E.Value, value : UUID.UUID }
        itemDecoder =
            D.map2 (\k v -> { key = k, value = v })
                (D.field "key" D.value)
                (D.field "value" UUID.jsonDecoder)
    in
    D.map7 Sequence
        (D.succeed id)
        (D.maybe <| D.field "_rev" D.string)
        (D.field "version" D.int)
        (D.field "kind" D.string)
        (D.field "title" D.string)
        (D.field "description" D.string)
        (D.field "items" (D.list itemDecoder))


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


personDecoder : String -> D.Decoder Person
personDecoder id =
    D.map4 Person
        (D.succeed id)
        (D.maybe <| D.field "_rev" D.string)
        (D.field "version" D.int)
        (D.field "names" (DE.dict2 D.int D.string))


descriptionDecoder : DescriptionId -> D.Decoder Description
descriptionDecoder id =
    D.map4 Description
        (D.succeed id)
        (D.field "_rev" <| D.nullable D.string)
        (D.field "version" D.int)
        (D.field "value" D.string)


noteDecoder : NoteId -> D.Decoder Note
noteDecoder id =
    D.map4 Note
        (D.succeed id)
        (D.maybe <| D.field "_rev" D.string)
        (D.field "version" D.int)
        (D.field "note" D.string)


tagDecoder : TagId -> D.Decoder Tag
tagDecoder id =
    D.map3 Tag
        (D.succeed id)
        (D.maybe <| D.field "_rev" D.string)
        (D.field "version" D.int)


propertyDecoder : PropertyId -> D.Decoder Property
propertyDecoder id =
    D.map3 Property
        (D.succeed id)
        (D.field "_rev" <| D.nullable D.string)
        (D.field "version" D.int)


linkDecoder : LinkId -> D.Decoder Link
linkDecoder id =
    D.map3 Link
        (D.succeed id)
        (D.field "_rev" <| D.nullable D.string)
        (D.field "version" D.int)


modificationDecoder : ModificationId -> D.Decoder Modification
modificationDecoder id =
    D.map6 Modification
        (D.succeed id)
        (D.maybe <| D.field "_rev" D.string)
        (D.field "version" D.int)
        (D.field "comment" D.string)
        (D.field "docversion" D.int)
        (D.field "value" D.value)


utilityDecoder : UtilityId -> D.Decoder Utility
utilityDecoder id =
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

        MySequence sequence ->
            sequenceEncoder sequence

        MyPage page ->
            pageEncoder page

        MyTag tag ->
            tagEncoder tag

        MyNote note ->
            noteEncoder note

        MyProperty property ->
            propertyEncoder property

        MyUtility utility ->
            utilityEncoder utility

        MyModification modification ->
            modificationEncoder modification

        MyDescription description ->
            descriptionEncoder description

        MyLink link ->
            linkEncoder link


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


sequenceEncoder : Sequence -> E.Value
sequenceEncoder seq =
    let
        itemEncoder : { key : E.Value, value : UUID.UUID } -> E.Value
        itemEncoder item =
            E.object
                [ ( "key", item.key )
                , ( "value", E.string (UUID.toString item.value) )
                ]
    in
    [ ( "_id", E.string (docIdToString <| SequenceId seq.id) )
    , ( "version", E.int seq.version )
    , ( "kind", E.string seq.kind )
    , ( "title", E.string seq.title )
    , ( "description", E.string seq.description )
    , ( "items", E.list itemEncoder seq.items )
    ]
        |> addRev seq.rev


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


pageEncoder : Page -> E.Value
pageEncoder page =
    [ ( "_id", E.string (docIdToString <| PageId page.id) )
    , ( "version", E.int page.version )
    , ( "title", E.string page.title )
    , ( "description", E.string page.description )
    ]
        |> addRev page.rev


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


noteEncoder : Note -> E.Value
noteEncoder note =
    [ ( "_id", E.string (noteIdToString note.id) )
    , ( "version", E.int note.version )
    , ( "note", E.string note.note )
    ]
        |> addRev note.rev


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


linkEncoder : Link -> E.Value
linkEncoder link =
    [ ( "_id", E.string (linkIdToString link.id) )
    , ( "version", E.int link.version )
    ]
        |> addRev link.rev


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
compositeBuilder v comp =
    case v of
        MyTag t ->
            { comp | tags = t :: comp.tags }

        MyProperty p ->
            { comp | properties = p :: comp.properties }

        MyDescription d ->
            { comp | descriptions = d :: comp.descriptions }

        MyModification m ->
            { comp | modifications = m :: comp.modifications }

        MyLink l ->
            { comp | links = l :: comp.links }

        MyUtility _ ->
            comp

        other ->
            { comp | doc = Just other }
