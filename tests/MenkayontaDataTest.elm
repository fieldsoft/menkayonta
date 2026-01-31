module MenkayontaDataTest exposing (suite)

import Expect exposing (Expectation)
import Menkayonta exposing (..)
import Random
import Test exposing (..)
import Time
import UUID


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
        , test "Tag without fragment" <|
            \_ ->
                Expect.equal
                    ({ kind = "tagname"
                     , docid = intid
                     , fragment = []
                     }
                    |> MyTagId |> Just
                    )
                    (String.join "/" [ "interlinear"
                                     , UUID.toString uuid
                                     , "tag"
                                     , "tagname"
                                     ]
                        |> stringToIdentifier
                    )      
        , test "Description without fragment" <|
            \_ ->
                Expect.equal
                    ({ kind = "descriptionname"
                     , docid = intid
                     , fragment = []
                     }
                    |> MyDescriptionId |> Just
                    )
                    (String.join "/" [ "interlinear"
                                     , UUID.toString uuid
                                     , "description"
                                     , "descriptionname"
                                     ]
                        |> stringToIdentifier
                    )      
        , test "Utility without fragment" <|
            \_ ->
                Expect.equal
                    ({ kind = "utilname"
                     , docid = intid
                     , fragment = []
                     }
                    |> MyUtilityId |> Just
                    )
                    (String.join "/" [ "utility"
                                     , "utilname"
                                     , "interlinear"
                                     , UUID.toString uuid
                                     ]
                        |> stringToIdentifier
                    )      
        , test "Property without fragment" <|
            \_ ->
                Expect.equal
                    ({ kind = "propname"
                     , value = "propvalue"
                     , docid = intid
                     , fragment = []
                     }
                    |> MyPropertyId |> Just
                    )
                    (String.join "/" [ "interlinear"
                                     , UUID.toString uuid
                                     , "property"
                                     , "propname"
                                     , "propvalue"
                                     ]
                        |> stringToIdentifier
                    )      
        , test "Modification without fragment" <|
            \_ ->
                Expect.equal
                    ({ kind = "modname"
                     , person = PersonId "e@w.com"
                     , time = Time.millisToPosix 0
                     , docid = intid
                     , fragment = []
                     }
                    |> MyModificationId |> Just
                    )
                    (String.join "/" [ "interlinear"
                                     , UUID.toString uuid
                                     , "modification"
                                     , "modname"
                                     , "0"
                                     , "e@w.com"
                                     ]
                        |> stringToIdentifier
                    )      
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
                    (String.join "/" [ "person", "e@w.com" ])
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
                     , fragment = []
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
                     , fragment = []
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
                     , fragment = []
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
                        , "e@w.com"
                        ]
                    )
                    ({ kind = "modname"
                     , time = Time.millisToPosix 0
                     , docid = intid
                     , person = PersonId "e@w.com"
                     , fragment = []
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
                     , fragment = []
                     }
                        |> MyUtilityId
                        |> identifierToString
                    )
        ]
