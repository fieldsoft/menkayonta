module MenkayontaDataTest exposing (suite)

import Expect exposing (Expectation)
import Menkayonta
    exposing
        ( DocId
        , InterlinearId
        , MyDescriptionId
        , MyDocId
        , MyModificationId
        , MyPropertyId
        , MyTagId
        , MyUtilityId
        , PersonId
        , identifierToString
        , stringToIdentifier
        )
import Random
import Test exposing (Test, describe, test)
import Time
import UUID
import Url


uuid : UUID.UUID
uuid =
    Random.initialSeed 12345
        |> Random.step UUID.generator
        |> Tuple.first


intid : DocId
intid =
    InterlinearId uuid


suite : Test
suite =
    describe "Menkayonta Data Types"
        [ idToStringTests
        , stringToIdTests
        , reversals
        ]


stringToIdTests : Test
stringToIdTests =
    describe "string to identifier"
        [ test "Interlinear Doc" <|
            \_ ->
                Expect.equal
                    (intid |> MyDocId |> Just)
                    (String.join "/" [ "interlinear", UUID.toString uuid ]
                        |> stringToIdentifier
                    )
        , test "Person Doc" <|
            \_ ->
                Expect.equal
                    (PersonId "e@w.com" |> MyDocId |> Just)
                    (String.join "/" [ "person", "e@w.com" ]
                        |> stringToIdentifier
                    )
        , describe "without fragments"
            [ test "Tag" <|
                \_ ->
                    Expect.equal
                        ({ kind = "tagname"
                         , docid = intid
                         , fragment = Nothing
                         }
                            |> MyTagId
                            |> Just
                        )
                        (String.join "/"
                            [ "interlinear"
                            , UUID.toString uuid
                            , "tag"
                            , "tagname"
                            ]
                            |> stringToIdentifier
                        )
            , test "Utility" <|
                \_ ->
                    Expect.equal
                        ({ kind = "utilname"
                         , docid = intid
                         , fragment = Nothing
                         }
                            |> MyUtilityId
                            |> Just
                        )
                        (String.join "/"
                            [ "utility"
                            , "utilname"
                            , "interlinear"
                            , UUID.toString uuid
                            ]
                            |> stringToIdentifier
                        )
            , test "Property" <|
                \_ ->
                    Expect.equal
                        ({ kind = "propname"
                         , value = "propvalue"
                         , docid = intid
                         , fragment = Nothing
                         }
                            |> MyPropertyId
                            |> Just
                        )
                        (String.join "/"
                            [ "interlinear"
                            , UUID.toString uuid
                            , "property"
                            , "propname"
                            , "propvalue"
                            ]
                            |> stringToIdentifier
                        )
            , test "Modification" <|
                \_ ->
                    Expect.equal
                        ({ kind = "modname"
                         , person = PersonId "e@w.com"
                         , time = Time.millisToPosix 0
                         , docid = intid
                         , fragment = Nothing
                         }
                            |> MyModificationId
                            |> Just
                        )
                        (String.join "/"
                            [ "interlinear"
                            , UUID.toString uuid
                            , "modification"
                            , "modname"
                            , "0"
                            , "e@w.com"
                            ]
                            |> stringToIdentifier
                        )
            , test "Description" <|
                \_ ->
                    Expect.equal
                        ({ kind = "descriptionname"
                         , docid = intid
                         , fragment = Nothing
                         }
                            |> MyDescriptionId
                            |> Just
                        )
                        (String.join "/"
                            [ "interlinear"
                            , UUID.toString uuid
                            , "description"
                            , "descriptionname"
                            ]
                            |> stringToIdentifier
                        )
            ]
        , describe "with fragment"
            [ test "Tag" <|
                \_ ->
                    Expect.equal
                        ({ kind = "tagname"
                         , docid = intid
                         , fragment = Just "$.store.book[3].author"
                         }
                            |> MyTagId
                            |> Just
                        )
                        (String.join "/"
                            [ "interlinear"
                            , UUID.toString uuid
                            , "tag"
                            , "tagname"
                            ]
                            |> (\x -> x :: [ "$.store.book[3].author" ])
                            |> String.join "#"
                            |> stringToIdentifier
                        )
            , test "Description" <|
                \_ ->
                    Expect.equal
                        ({ kind = "descriptionname"
                         , docid = intid
                         , fragment = Just "$.store.book[3].author"
                         }
                            |> MyDescriptionId
                            |> Just
                        )
                        (String.join "/"
                            [ "interlinear"
                            , UUID.toString uuid
                            , "description"
                            , "descriptionname"
                            ]
                            |> (\x -> x :: [ "$.store.book[3].author" ])
                            |> String.join "#"
                            |> stringToIdentifier
                        )
            ]
        ]


idToStringTests : Test
idToStringTests =
    describe "identifier to string"
        [ test "Interlinear Doc" <|
            \_ ->
                Expect.equal
                    (String.join "/" [ "interlinear", UUID.toString uuid ])
                    (intid
                        |> MyDocId
                        |> identifierToString
                    )
        , test "Person Doc" <|
            \_ ->
                Expect.equal
                    (String.join "/"
                        [ "person"
                        , Url.percentEncode "e@w.com"
                        ]
                    )
                    (PersonId "e@w.com"
                        |> MyDocId
                        |> identifierToString
                    )
        , test "Tag without fragment" <|
            \_ ->
                Expect.equal
                    (String.join "/"
                        [ "interlinear"
                        , UUID.toString uuid
                        , "tag"
                        , "tagname"
                        ]
                    )
                    ({ kind = "tagname"
                     , docid = intid
                     , fragment = Nothing
                     }
                        |> MyTagId
                        |> identifierToString
                    )
        , test "Property without fragment" <|
            \_ ->
                Expect.equal
                    (String.join "/"
                        [ "interlinear"
                        , UUID.toString uuid
                        , "property"
                        , "propname"
                        , "propvalue"
                        ]
                    )
                    ({ kind = "propname"
                     , value = "propvalue"
                     , docid = intid
                     , fragment = Nothing
                     }
                        |> MyPropertyId
                        |> identifierToString
                    )
        , test "Description without fragment" <|
            \_ ->
                Expect.equal
                    (String.join "/"
                        [ "interlinear"
                        , UUID.toString uuid
                        , "description"
                        , "descriptionname"
                        ]
                    )
                    ({ kind = "descriptionname"
                     , docid = intid
                     , fragment = Nothing
                     }
                        |> MyDescriptionId
                        |> identifierToString
                    )
        , test "Modification without fragment" <|
            \_ ->
                Expect.equal
                    (String.join "/"
                        [ "interlinear"
                        , UUID.toString uuid
                        , "modification"
                        , "modname"
                        , "0"
                        , Url.percentEncode "e@w.com"
                        ]
                    )
                    ({ kind = "modname"
                     , time = Time.millisToPosix 0
                     , docid = intid
                     , person = PersonId "e@w.com"
                     , fragment = Nothing
                     }
                        |> MyModificationId
                        |> identifierToString
                    )
        , test "Utility without fragment" <|
            \_ ->
                Expect.equal
                    (String.join "/"
                        [ "utility"
                        , "utilname"
                        , "interlinear"
                        , UUID.toString uuid
                        ]
                    )
                    ({ kind = "utilname"
                     , docid = intid
                     , fragment = Nothing
                     }
                        |> MyUtilityId
                        |> identifierToString
                    )
        ]


reversals : Test
reversals =
    describe "Correct escaping"
        [ test "slash in the input" <|
            \_ ->
                Expect.equal
                    ({ kind = "mod/name"
                     , time = Time.millisToPosix 0
                     , docid = intid
                     , person = PersonId "e@w.com"
                     , fragment = Nothing
                     }
                        |> MyModificationId
                        |> Just
                    )
                    ({ kind = "mod/name"
                     , time = Time.millisToPosix 0
                     , docid = intid
                     , person = PersonId "e@w.com"
                     , fragment = Nothing
                     }
                        |> MyModificationId
                        |> identifierToString
                        |> stringToIdentifier
                    )
        ]
