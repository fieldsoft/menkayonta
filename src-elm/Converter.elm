port module Converter exposing (Flags, Model, Msg, Stage, main)

import DativeTypes as DT
import Dict exposing (Dict)
import Email exposing (Email)
import Json.Decode as D
import Json.Encode as E
import Json.Encode.Extra as EE
import Maybe.Extra as ME
import Menkayonta as M
import Random
import Task
import Time
import UUID exposing (UUID)


port receivedDativeForms : (E.Value -> msg) -> Sub msg


port sendBulkDocs : E.Value -> Cmd msg


port reportError : String -> Cmd msg


port reportInfo : String -> Cmd msg


type Msg
    = ReceivedDativeForms E.Value
    | Next


type alias Model =
    { seeds : UUID.Seeds
    , project : UUID
    , to : Dict String M.Value
    , from : List DT.DativeForm
    , stage : Stage
    , time : Time.Posix
    , me : String
    , people : Dict Int M.Person
    }


{-| Stages are used for organization of code. All stages could be done
at once, but I attempt to divide things by topic. In many cases the
central theme is some person, such as a speaker or modifier, who
performed some role with associated information.

What was a big table with some referenced information is broken up
into a number of independent documents, which reference a main
interlinear gloss document. In practice, most of these documents will
not be created because the original table had data that was very
sparse, with many nulls and empty strings.

One can think of the processing of the input dative json as similar to
a fold, from a list of dative items to a heterogeneous dictionary of
Menkayonta values. It is not designed to be particularly efficient. I
hope that it easy to read and maintain.

-}
type Stage
    = OriginalS -- Store the original docuemnt for future reference.
    | SpeakerS -- speaker related information.
    | ElicitorS -- elicitation related information.
    | EntererS -- information associated with the initial entry.
    | ModifierS -- information associated with a subsequent modification.
    | VerifierS -- information associated with a verification.
    | UtilityS -- information associated with a parsing utility writen
      -- for dative.
    | InterlinearS -- the main document and remaining metadata


{-| Random seeds are needed to generate Person documents. One of the
reasons that each stage of processing focuses on a particular user is
that each stage potentially generates a Person and may update the seed
values.
-}
type alias Flags =
    { seed1 : Int
    , seed2 : Int
    , seed3 : Int
    , seed4 : Int
    , me : String
    , project : String
    , time : Int
    }


{-| The Job object just holds the JSON Values that get passed back and
forth through the port.
-}
type alias Job =
    { project : UUID
    , payload : E.Value
    }


{-| There was a parser utility embedded in Dative that didn't see much
use, and so has not had its functionality reproduced in Menkayonta,
yet. The notion of Utility is supposed to serve as a place to hold
data structures that may be specific for similar utilities in
Menkayonta. In this case, it may ultimately simply serve as a stash
for the legacy data.
-}
type alias DativeUtilityValue =
    { version : Int
    , syntactic_category_string : Maybe String
    , morpheme_break_ids : Maybe (List DT.DativeToken)
    , morpheme_gloss_ids : Maybe (List DT.DativeToken)
    , break_gloss_category : Maybe String
    }


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        seeds : UUID.Seeds
        seeds =
            { seed1 = Random.initialSeed flags.seed1
            , seed2 = Random.initialSeed flags.seed2
            , seed3 = Random.initialSeed flags.seed3
            , seed4 = Random.initialSeed flags.seed4
            }

        validMe : Maybe Email
        validMe =
            Email.fromString flags.me

        project : Result UUID.Error UUID.UUID
        project =
            UUID.fromString flags.project
    in
    case ( validMe, project ) of
        ( Just _, Ok project_ ) ->
            ( { seeds = seeds
              , project = project_
              , to = Dict.empty
              , from = []
              , stage = OriginalS
              , time = Time.millisToPosix flags.time
              , me = flags.me
              , people = Dict.empty
              }
            , reportInfo "Converter Initialized"
            )

        ( Just _, Err _ ) ->
            let
                fakeuuid : UUID.UUID
                fakeuuid =
                    case UUID.step seeds of
                        ( uuid, _ ) ->
                            uuid
            in
            ( { seeds = seeds
              , project = fakeuuid
              , to = Dict.empty
              , from = []
              , stage = OriginalS
              , time = Time.millisToPosix 0
              , me = flags.me
              , people = Dict.empty
              }
            , reportError "The project flag is an invalid UUID."
            )

        ( Nothing, Ok project_ ) ->
            ( { seeds = seeds
              , project = project_
              , to = Dict.empty
              , from = []
              , stage = OriginalS
              , time = Time.millisToPosix 0
              , me = "nobody@example.com"
              , people = Dict.empty
              }
            , reportError "The person flag is not a valid email address."
            )

        ( Nothing, Err _ ) ->
            let
                fakeuuid : UUID.UUID
                fakeuuid =
                    case UUID.step seeds of
                        ( uuid, _ ) ->
                            uuid
            in
            ( { seeds = seeds
              , project = fakeuuid
              , to = Dict.empty
              , from = []
              , stage = OriginalS
              , time = Time.millisToPosix 0
              , me = "nobody@example.com"
              , people = Dict.empty
              }
            , reportError "The project and person flags are invalid."
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedDativeForms job ->
            case D.decodeValue jobDecoder job of
                Err e ->
                    let
                        error : String
                        error =
                            D.errorToString e
                    in
                    ( model, reportError error )

                Ok j ->
                    -- The first pass only decodes the job, not the
                    -- payload.
                    case D.decodeValue (D.list DT.decoder) j.payload of
                        Err e_ ->
                            let
                                error : String
                                error =
                                    D.errorToString e_
                            in
                            ( model, reportError error )

                        Ok dts ->
                            ( { model | from = dts }
                            , next "Received Dative Forms"
                            )

        Next ->
            case model.from of
                -- An empty list indicates that processing has
                -- finished.
                [] ->
                    let
                        job : Job
                        job =
                            { project =
                                model.project
                            , payload =
                                Dict.values model.to
                                    |> List.map M.encoder
                                    |> E.list identity
                            }

                        jval : E.Value
                        jval =
                            jobEncoder job
                    in
                    ( { model | stage = OriginalS }
                    , Cmd.batch
                        [ sendBulkDocs jval
                        , reportInfo "Completed Processing"
                        ]
                    )

                -- Take the head of the list and process the current
                -- stage.
                curr :: _ ->
                    resolveStage curr model


resolveStage : DT.DativeForm -> Model -> ( Model, Cmd Msg )
resolveStage curr model =
    let
        docid : M.DocId
        docid =
            M.InterlinearId curr.uuid
    in
    case model.stage of
        -- At this stage, the original dative 'form' object is saved
        -- in a `modification` document. This makes it available for
        -- later examination if needed. In most other cases there is a
        -- stage handling function associated with the stage, but
        -- OriginalS is fairly simple.
        OriginalS ->
            let
                constm : ConstructM
                constm =
                    { kind = "importsource"
                    , time = model.time
                    , docid = docid
                    , pid = model.me
                    , json = DT.encoder curr
                    }

                m : IdVal
                m =
                    constructModifier constm

                newto : Dict String M.Value
                newto =
                    Dict.insert (M.identifierToString m.id) m.val model.to
            in
            ( { model | to = newto, stage = SpeakerS }
            , next "Completed OriginalS"
            )

        -- The stages generally correspond to People-ish objects.
        SpeakerS ->
            case curr.speaker of
                Just speaker ->
                    speakerStage
                        { docid = docid
                        , curr = curr
                        , speaker = speaker
                        , model = model
                        }

                Nothing ->
                    -- If there wasn't a speaker, move on to the next
                    -- item.
                    ( { model | stage = ElicitorS }, next "Skip SpeakerS" )

        ElicitorS ->
            case curr.elicitor of
                Just elicitor ->
                    elicitorStage
                        { docid = docid
                        , curr = curr
                        , elicitor = elicitor
                        , model = model
                        }

                Nothing ->
                    -- If there wasn't an elicitor, move on to the next
                    -- item.
                    ( { model | stage = EntererS }, next "Skip ElicitorS" )

        EntererS ->
            case curr.enterer of
                Just enterer ->
                    entererStage
                        { docid = docid
                        , curr = curr
                        , enterer = enterer
                        , model = model
                        }

                Nothing ->
                    -- If there wasn't an enterer, move on to the next
                    -- item.
                    ( { model | stage = ModifierS }, next "Skip EntererS" )

        ModifierS ->
            case curr.modifier of
                Just modifier ->
                    modifierStage
                        { docid = docid
                        , curr = curr
                        , modifier = modifier
                        , model = model
                        }

                Nothing ->
                    -- If there wasn't a modifier, move on to the next
                    -- item.
                    ( { model | stage = VerifierS }, next "Skip ModifierS" )

        VerifierS ->
            case curr.verifier of
                Just verifier ->
                    verifierStage
                        { docid = docid
                        , curr = curr
                        , verifier = verifier
                        , model = model
                        }

                Nothing ->
                    -- If there wasn't a verifier, move on to the next
                    -- item.
                    ( { model | stage = UtilityS }, next "Skip VerifierS" )

        UtilityS ->
            let
                utilityVal : DativeUtilityValue
                utilityVal =
                    { version =
                        1
                    , syntactic_category_string =
                        curr.syntactic_category_string
                    , morpheme_break_ids =
                        curr.morpheme_break_ids
                    , morpheme_gloss_ids =
                        curr.morpheme_gloss_ids
                    , break_gloss_category =
                        curr.break_gloss_category
                    }

                empty : DativeUtilityValue
                empty =
                    DativeUtilityValue 1 Nothing Nothing Nothing Nothing
            in
            -- If the utility document will not be preserving any
            -- information, do not create it.
            if utilityVal == empty then
                ( { model | stage = InterlinearS }, next "Skip UtilityS" )

            else
                utilityStage
                    { docid = docid
                    , utilityVal = dativeUtilEncoder utilityVal
                    , model = model
                    }

        InterlinearS ->
            interlinearStage curr model


interlinearStage : DT.DativeForm -> Model -> ( Model, Cmd Msg )
interlinearStage curr model =
    let
        interlinear : M.Interlinear
        interlinear =
            { id = curr.uuid
            , rev = Nothing
            , version = 1
            , text = curr.transcription
            , ann =
                { breaks = curr.morpheme_break
                , glosses = curr.morpheme_gloss
                , phonemic = curr.phonetic_transcription
                , judgment = curr.grammaticality
                }
            , translations =
                List.map
                    (\t ->
                        ( t.id
                        , { translation = t.transcription
                          , judgment = t.grammaticality
                          }
                        )
                    )
                    curr.translations
                    |> Dict.fromList
            }

        interlineardoc : M.Value
        interlineardoc =
            M.MyInterlinear interlinear

        docid : M.DocId
        docid =
            curr.uuid |> M.InterlinearId

        stringid : String
        stringid =
            docid |> M.MyDocId |> M.identifierToString

        nptrans : Maybe IdVal
        nptrans =
            constNonBlankDesc "narrow phonetic transription"
                curr.narrow_phonetic_transcription
                docid

        syntax : Maybe IdVal
        syntax =
            constNonBlankProp "syntax" curr.syntax docid

        semantics : Maybe IdVal
        semantics =
            constNonBlankProp "semantics" curr.semantics docid

        status : Maybe IdVal
        status =
            constNonBlankProp "status" curr.status docid

        syntactic_category : Maybe IdVal
        syntactic_category =
            maybeConstProp "syntactic category"
                (curr.syntactic_category |> Maybe.map .name)
                docid

        source : Maybe IdVal
        source =
            maybeConstProp "source" curr.source docid

        tags : List IdVal
        tags =
            List.map (\t -> constructTag t.name docid) curr.tags

        newto : Dict String M.Value
        newto =
            Dict.insert stringid interlineardoc model.to
                |> (\indict ->
                        List.foldl
                            (\t indict_ ->
                                Dict.insert
                                    (M.identifierToString t.id)
                                    t.val
                                    indict_
                            )
                            indict
                            tags
                   )
                |> when nptrans
                |> when syntax
                |> when semantics
                |> when status
                |> when syntactic_category
                |> when source
    in
    ( { model
        | stage = OriginalS
        , to = newto
        , from = tail model.from
      }
    , next "Completed InterlinearS"
    )


utilityStage :
    { docid : M.DocId
    , utilityVal : E.Value
    , model : Model
    }
    -> ( Model, Cmd Msg )
utilityStage { docid, utilityVal, model } =
    let
        utility : M.Utility
        utility =
            { id =
                { kind = "dative"
                , docid = docid
                , fragment = Nothing
                }
            , rev = Nothing
            , version = 1
            , value = utilityVal
            }

        stringid : String
        stringid =
            M.identifierToString (M.MyUtilityId utility.id)

        newto : Dict String M.Value
        newto =
            Dict.insert stringid (M.MyUtility utility) model.to
    in
    ( { model
        | to = newto
        , stage = InterlinearS
      }
    , next "Completed UtilityS"
    )


verifierStage :
    { docid : M.DocId
    , curr : DT.DativeForm
    , verifier : DT.DativePerson
    , model : Model
    }
    -> ( Model, Cmd Msg )
verifierStage { docid, curr, verifier, model } =
    let
        p : StringIdPerson
        p =
            perhapsMakePerson verifier model

        people : Dict Int M.Person
        people =
            Dict.insert verifier.id p.person model.people

        -- The original software did not record a date for this
        -- modification type. I am defaulting the last modification.
        modDate_ : Maybe IdVal
        modDate_ =
            modDate "verified"
                (Just curr.datetime_modified)
                docid
                p.person.id

        newto : Dict String M.Value
        newto =
            Dict.insert p.stringId (M.MyPerson p.person) model.to
                |> when modDate_
    in
    ( { model
        | people = people
        , to = newto
        , stage = UtilityS
      }
    , next "Completed VerifierS"
    )


modifierStage :
    { docid : M.DocId
    , curr : DT.DativeForm
    , modifier : DT.DativePerson
    , model : Model
    }
    -> ( Model, Cmd Msg )
modifierStage { docid, curr, modifier, model } =
    let
        p : StringIdPerson
        p =
            perhapsMakePerson modifier model

        people : Dict Int M.Person
        people =
            Dict.insert modifier.id p.person model.people

        -- Add a modification event for the elcitation
        -- date, if it exists.
        modDate_ : Maybe IdVal
        modDate_ =
            modDate "updated"
                (Just curr.datetime_modified)
                docid
                p.person.id

        newto : Dict String M.Value
        newto =
            Dict.insert p.stringId (M.MyPerson p.person) model.to
                |> when modDate_
    in
    ( { model
        | people = people
        , to = newto
        , stage = VerifierS
      }
    , next "Completed ModiferS"
    )


entererStage :
    { docid : M.DocId
    , curr : DT.DativeForm
    , enterer : DT.DativePerson
    , model : Model
    }
    -> ( Model, Cmd Msg )
entererStage { docid, curr, enterer, model } =
    let
        p : StringIdPerson
        p =
            perhapsMakePerson enterer model

        people : Dict Int M.Person
        people =
            Dict.insert enterer.id p.person model.people

        -- Add a modification event for the elcitation
        -- date, if it exists.
        modDate_ : Maybe IdVal
        modDate_ =
            modDate "entered" (Just curr.datetime_entered) docid p.person.id

        newto : Dict String M.Value
        newto =
            Dict.insert p.stringId (M.MyPerson p.person) model.to
                |> when modDate_
    in
    ( { model
        | people = people
        , to = newto
        , stage = ModifierS
      }
    , next "Completed EntererS"
    )


elicitorStage :
    { docid : M.DocId
    , curr : DT.DativeForm
    , elicitor : DT.DativePerson
    , model : Model
    }
    -> ( Model, Cmd Msg )
elicitorStage { docid, curr, elicitor, model } =
    let
        p : StringIdPerson
        p =
            perhapsMakePerson elicitor model

        people : Dict Int M.Person
        people =
            Dict.insert elicitor.id p.person model.people

        -- Associate speaker comment information with
        -- the interlinear document.
        comment_ : Maybe IdVal
        comment_ =
            constNonBlankDesc "comment" curr.comments docid

        -- Add a modification event for the elcitation
        -- date, if it exists.
        modDate_ : Maybe IdVal
        modDate_ =
            modDate "elicitation" curr.date_elicited docid p.person.id

        elicitationMethod : Maybe IdVal
        elicitationMethod =
            maybeConstProp "elicitation method"
                (curr.elicitation_method
                    |> Maybe.map .name
                )
                docid

        newto : Dict String M.Value
        newto =
            Dict.insert p.stringId (M.MyPerson p.person) model.to
                |> when comment_
                |> when modDate_
                |> when elicitationMethod
    in
    ( { model
        | people = people
        , to = newto
        , stage = EntererS
      }
    , next "Completed ElicitorS"
    )


speakerStage :
    { docid : M.DocId
    , curr : DT.DativeForm
    , speaker : DT.DativeSpeaker
    , model : Model
    }
    -> ( Model, Cmd Msg )
speakerStage { docid, curr, speaker, model } =
    let
        p : StringIdPerson
        p =
            case Dict.get -speaker.id model.people of
                Just person ->
                    { stringId = personIdString person.id
                    , person = person
                    }

                Nothing ->
                    constructPerson
                        { first = speaker.first_name
                        , last = speaker.last_name
                        , id = speaker.id
                        }

        -- Add speakers with negative numbers to avoid
        -- collisions with legacy person type.
        people : Dict Int M.Person
        people =
            Dict.insert -speaker.id p.person model.people

        personid : M.DocId
        personid =
            M.PersonId p.person.id

        -- Associate dialect information included with
        -- the dative speaker record with the
        -- interlinear document.
        docDialect : Maybe IdVal
        docDialect =
            maybeConstProp "dialect" speaker.dialect docid

        -- Associate dialect information included with
        -- the dative speaker record with the
        -- speaker document.
        speakerDialect : Maybe IdVal
        speakerDialect =
            maybeConstProp "dialect" speaker.dialect personid

        -- Associate speaker comment information with
        -- the interlinear document.
        speakerComment : Maybe IdVal
        speakerComment =
            constNonBlankDesc "speaker comment" curr.speaker_comments docid

        -- Sequentially insert new records to the
        -- output "to" dictionary.
        newto : Dict String M.Value
        newto =
            Dict.insert p.stringId (M.MyPerson p.person) model.to
                |> when speakerDialect
                |> when docDialect
                |> when speakerComment
    in
    ( { model
        | to = newto
        , people = people
        , stage = ElicitorS
      }
    , next "Completed SpeakerS"
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    receivedDativeForms ReceivedDativeForms


jobDecoder : D.Decoder Job
jobDecoder =
    D.map2 Job
        (D.field "project" UUID.jsonDecoder)
        (D.field "payload" D.value)


jobEncoder : Job -> E.Value
jobEncoder job =
    E.object
        [ ( "project", UUID.toValue job.project )
        , ( "payload", job.payload )
        ]


when : Maybe IdVal -> Dict String M.Value -> Dict String M.Value
when input default =
    ME.unwrap default
        (\x -> Dict.insert (M.identifierToString x.id) x.val default)
        input


constructTag : String -> M.DocId -> IdVal
constructTag kind docid =
    { id =
        { kind = kind
        , docid = docid
        , fragment = Nothing
        }
    , rev = Nothing
    , version = 1
    }
        |> (\x -> { id = M.MyTagId x.id, val = M.MyTag x })


constructProperty : String -> String -> M.DocId -> IdVal
constructProperty kind str docid =
    { id =
        { kind = kind
        , value = str
        , docid = docid
        , fragment = Nothing
        }
    , rev = Nothing
    , version = 1
    }
        |> (\x ->
                { id = M.MyPropertyId x.id
                , val = M.MyProperty x
                }
           )


maybeConstProp : String -> Maybe String -> M.DocId -> Maybe IdVal
maybeConstProp kind str docid =
    str |> Maybe.map (\x -> constructProperty kind x docid)


constNonBlankProp : String -> String -> M.DocId -> Maybe IdVal
constNonBlankProp kind str docid =
    if String.length str == 0 then
        Nothing

    else
        Just <| constructProperty kind str docid


constructDescription : String -> String -> M.DocId -> IdVal
constructDescription kind value docid =
    { id =
        { kind = kind
        , docid = docid
        , fragment = Nothing
        }
    , rev = Nothing
    , version = 1
    , value = value
    }
        |> (\x ->
                { id = M.MyDescriptionId x.id
                , val = M.MyDescription x
                }
           )


constNonBlankDesc : String -> String -> M.DocId -> Maybe IdVal
constNonBlankDesc kind c docid =
    if String.length c > 0 then
        constructDescription kind c docid |> Just

    else
        Nothing


personIdString : String -> String
personIdString str =
    str
        |> M.PersonId
        |> M.MyDocId
        |> M.identifierToString


{-| The person will only be constructed if they haven't been already.
-}
perhapsMakePerson : DT.DativePerson -> Model -> StringIdPerson
perhapsMakePerson dperson model =
    case Dict.get dperson.id model.people of
        Just person ->
            { stringId = personIdString person.id
            , person = person
            }

        Nothing ->
            constructPerson
                { first = dperson.first_name
                , last = dperson.last_name
                , id = dperson.id
                }


type alias StringIdPerson =
    { stringId : String
    , person : M.Person
    }


constructPerson : { first : String, last : String, id : Int } -> StringIdPerson
constructPerson pdata =
    let
        first_name : String
        first_name =
            pdata.first |> String.trim

        last_name : String
        last_name =
            pdata.last |> String.trim

        name : String
        name =
            String.join " " [ first_name, last_name ]

        email : String
        email =
            first_name ++ "." ++ last_name ++ "@example.com"

        stringid : String
        stringid =
            personIdString email

        newperson : M.Person
        newperson =
            { id = email
            , rev = Nothing
            , version = 1
            , names = Dict.singleton pdata.id name
            }
    in
    { stringId = stringid
    , person = newperson
    }


modDate : String -> Maybe Time.Posix -> M.DocId -> String -> Maybe IdVal
modDate kind time docid pid =
    time
        |> Maybe.map
            (\ed ->
                constructModifier
                    { kind = kind
                    , time = ed
                    , docid = docid
                    , pid = pid
                    , json = E.null
                    }
            )


type alias ConstructM =
    { kind : String
    , time : Time.Posix
    , docid : M.DocId
    , pid : String
    , json : E.Value
    }


type alias IdVal =
    { id : M.Identifier
    , val : M.Value
    }


constructModifier : ConstructM -> IdVal
constructModifier { kind, time, docid, pid, json } =
    { id =
        { kind = kind
        , docid = docid
        , time = time
        , person = M.PersonId pid
        , fragment = Nothing
        }
    , rev = Nothing
    , version = 1
    , comment = "bulk import"
    , docversion = 0
    , value = json
    }
        |> (\mod ->
                { id = M.MyModificationId mod.id
                , val = M.MyModification mod
                }
           )


dativeUtilEncoder : DativeUtilityValue -> E.Value
dativeUtilEncoder du =
    E.object
        [ ( "version"
          , E.int du.version
          )
        , ( "syntactic_category_string"
          , EE.maybe E.string du.syntactic_category_string
          )
        , ( "morpheme_break_ids"
          , EE.maybe (E.list <| DT.dativeTokenEncoder) du.morpheme_break_ids
          )
        , ( "morpheme_gloss_ids"
          , EE.maybe (E.list <| DT.dativeTokenEncoder) du.morpheme_gloss_ids
          )
        , ( "break_gloss_category"
          , EE.maybe E.string du.break_gloss_category
          )
        ]


tail : List a -> List a
tail list =
    case list of
        [] ->
            []

        _ :: tl ->
            tl


next : String -> Cmd Msg
next msg =
    Cmd.batch
        [ Task.perform identity <| Task.succeed Next
        , reportInfo msg
        ]
