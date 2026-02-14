module Form.Interlinear exposing
    ( Model
    , Msg(..)
    , init
    , initData
    , update
    , view
    )

import Dict
import Form.Shared
    exposing
        ( FieldDescription
        , SelectField
        , StringField
        , blankSelect
        , blankString
        , displayField
        )
import Html
import Html.Attributes as Attr
import Html.Events as Event
import List.Extra as LE
import Menkayonta
    exposing
        ( DocId(..)
        , Identifier(..)
        , identifierToString
        )
import UUID


{-| The Model describes a form representation of the Interlinear type
from Menkayonta.
-}
type alias Model =
    { id : UUID.UUID
    , rev : Maybe String
    , version : Int
    , changed : Bool
    , submitted : Bool
    , error : String
    , valid : Bool
    , text : StringField
    , annotations : Annotations
    , translations : List Translation
    , counter : Int
    }


{-| The `Msg` type largely deals with the fields of the form. Buttons
are considered a field, as are groupings, where Trans represents the
group of translations.
-}
type Msg
    = Text String
    | Breaks String 
    | Glosses String
    | Phonemic String
    | Judgment String
    | Trans String
    | TTranslation Int String
    | TJudgment Int String
    | Save
    | Cancel
    | None
 

{-| Annotations are the non-translation annotations.
-}
type alias Annotations =
    { breaks : StringField
    , glosses : StringField
    , phonemic : StringField
    , judgment : StringField
    }


{-| Translations occur in a list in the form.
-}
type alias Translation =
    { id : Int
    , deleted : Bool
    , translation : StringField
    , judgment : StringField
    }


{-| The initialization function. This will likely be refactored to
place more of the initialization logic in the function.
-}
init : Menkayonta.Interlinear -> ( Model, Cmd Msg )
init int =
    let
        ann : Annotations
        ann =
            { breaks =
                { value = int.ann.breaks
                , valid = True
                , error = ""
                , changed = False
                , original = int.ann.breaks
                }
            , glosses =
                { value = int.ann.glosses
                , valid = True
                , error = ""
                , changed = False
                , original = int.ann.glosses
                }
            , phonemic =
                { value = int.ann.phonemic
                , valid = True
                , error = ""
                , changed = False
                , original = int.ann.phonemic
                }
            , judgment =
                { value = int.ann.judgment
                , valid = True
                , error = ""
                , changed = False
                , original = int.ann.judgment
                }
            }

        trans_ :
            ( Int, Menkayonta.Translation )
            -> Translation
        trans_ ( k, v ) =
            { id = k
            , deleted = False
            , translation =
                { value = v.translation
                , valid = True
                , error = ""
                , changed = False
                , original = v.translation
                }
            , judgment =
                { value = v.judgment
                , valid = True
                , error = ""
                , changed = False
                , original = v.judgment
                }
            }

        trans : List Translation
        trans =
            Dict.toList int.translations
                |> List.map trans_

        counter : Int
        counter =
            Dict.keys int.translations
                |> List.maximum
                |> Maybe.withDefault 0

        model : Model
        model =
            { id = int.id
            , rev = int.rev
            , version = int.version
            , changed = False
            , submitted = False
            , error = ""
            , valid = True
            , text =
                { value = int.text
                , valid = True
                , error = ""
                , changed = False
                , original = int.text
                }
            , annotations = ann
            , translations = trans
            , counter = counter
            }
    in
    ( model, Cmd.none )


{-| A starter Data instance.
-}
initData : UUID.UUID -> Model
initData id =
    { id = id
    , rev = Nothing
    , version = 1
    , changed = False
    , submitted = False
    , error = "Please fill the empty form."
    , valid = False
    , text = { blankString | valid = False, error = "Cannot be empty." }
    , annotations =
        { breaks = blankString
        , glosses = blankString
        , phonemic = blankString
        , judgment = blankString
        }
    , translations = []
    , counter = 0
    }


{-| This handles events specific to particular fields.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        tokens : String -> Int
        tokens s =
            String.words s
                -- Don't include the empty string
                |> List.filter (\x -> String.length x > 0)
                |> List.length

        breax : String -> List Int
        breax s =
            String.words s
                -- Don't include the empty string
                |> List.filter (\x -> String.length x > 0)
                |> List.map (\x -> List.length <| String.split "-" x)

        breakMismatch :
            String
            -> String
            ->
                Maybe
                    { token1 : String
                    , token2 : String
                    , length1 : Int
                    , length2 : Int
                    }
        breakMismatch s1 s2 =
            let
                brx1 : List Int
                brx1 =
                    breax s1

                brx2 : List Int
                brx2 =
                    breax s2

                -- Find the first index where the number of breaks is
                -- unequal.
                idx : Maybe Int
                idx =
                    List.map2 (\a1 a2 -> a1 == a2) brx1 brx2
                        |> LE.findIndex not

                -- Get the token from the first list of tokens (if it
                -- exists) that had an unequal number of breaks to
                -- some item in the second list of tokens.
                token1 : Maybe String
                token1 =
                    Maybe.andThen (\i -> LE.getAt i (String.words s1)) idx

                token2 : Maybe String
                token2 =
                    Maybe.andThen (\i -> LE.getAt i (String.words s2)) idx

                -- Get the lenth differences between the tokens.
                length1 : Maybe Int
                length1 =
                    Maybe.andThen (\i -> LE.getAt i brx1) idx

                length2 : Maybe Int
                length2 =
                    Maybe.andThen (\i -> LE.getAt i brx2) idx
            in
            Maybe.map4
                (\t1 t2 l1 l2 ->
                    { token1 = t1
                    , token2 = t2
                    , length1 = l1
                    , length2 = l2
                    }
                )
                token1
                token2
                length1
                length2

        text : StringField
        text =
            model.text

        annotations : Annotations
        annotations =
            model.annotations

        translations : List Translation
        translations =
            model.translations

        judgment : StringField
        judgment =
            annotations.judgment

        breaks : StringField
        breaks =
            annotations.breaks

        glosses : StringField
        glosses =
            annotations.glosses

        phonemic : StringField
        phonemic =
            annotations.phonemic

        -- Return false if there are any invalid translation items.
        translationsValid : List Translation -> Bool
        translationsValid translations_ =
            translations_
                |> List.map
                    (\x ->
                         -- The translations are not required. So, any
                         -- deleted translation is valid.
                         if x.deleted then
                             [ True ]

                         else
                             [ x.translation.valid, x.judgment.valid ]
                    )
                |> List.concat
                |> List.all identity

        -- Return false if there are any invalid items
        valid : Model -> Bool
        valid d_ =
            List.all identity
                [ d_.text.valid
                , d_.annotations.judgment.valid
                , d_.annotations.breaks.valid
                , d_.annotations.glosses.valid
                , d_.annotations.phonemic.valid
                , translationsValid d_.translations
                ]

        toperr : String
        toperr =
            "Correct errors before saving."

        -- Set top level validity and error messages accordingly.
        validate : Model -> Model
        validate model_ =
            let
                valid_ : Bool
                valid_ =
                    valid model_
            in
            { model_
                | changed = True
                , valid = valid_
                , error =
                    if valid_ then
                        ""

                    else
                        toperr
            }

        -- The reason for dividing the list in this way is ensuring
        -- that the user doesn't have the elements of the user
        -- interface jump around while editing. It will also preserve
        -- the order from the Dict the items were derived from. See
        -- below for example usage.
        divided :
            Int
            ->
                Maybe
                    ( List Translation
                    , Translation
                    , List Translation
                    )
        divided id_ =
            LE.splitWhen (\x -> x.id == id_) translations
                |> Maybe.andThen
                    (\( x, y ) ->
                        List.head y
                            |> Maybe.andThen
                                (\h ->
                                    List.tail y
                                        |> Maybe.map
                                            (\z -> ( x, h, z ))
                                )
                    )

        textTokens : Int
        textTokens =
            tokens model.text.value
    in
    case msg of
        -- It is sometimes useful to have a non-operation.
        None ->
            ( model, Cmd.none )

        Text str ->
            if String.isEmpty str then
                ( validate
                    { model
                        | text =
                            { text
                                | value = str
                                , valid = False
                                , error = "The text can't be blank."
                                , changed = True
                            }
                    }
                , Cmd.none
                )

            else
                ( validate
                    { model
                        | text =
                            { text
                                | value = str
                                , valid = True
                                , error = ""
                                , changed = True
                            }
                    }
                , Cmd.none
                )

        Breaks str ->
            let
                newTokens : Int
                newTokens =
                    tokens str

                ok : Model
                ok =
                    { model
                        | annotations =
                            { annotations
                                | breaks =
                                    { breaks
                                        | value = str
                                        , valid = True
                                        , error = ""
                                        , changed = True
                                    }
                            }
                    }
                        |> validate
            in
            if String.isEmpty (String.trim str) then
                ( ok, Cmd.none )

            else if newTokens == textTokens then
                ( ok, Cmd.none )

            else
                let
                    err : String
                    err =
                        [ "The number of tokens is not equal."
                        , "The text has"
                        , String.fromInt textTokens ++ "."
                        , "This line has"
                        , String.fromInt newTokens ++ "."
                        ]
                            |> String.join " "
                in
                ( validate
                    { model
                        | annotations =
                            { annotations
                                | breaks =
                                    { breaks
                                        | value = str
                                        , valid = False
                                        , error = err
                                        , changed = True
                                    }
                            }
                    }
                , Cmd.none
                )

        Glosses str ->
            let
                newTokens : Int
                newTokens =
                    tokens str

                ok : Model
                ok =
                    { model
                        | annotations =
                            { annotations
                                | glosses =
                                    { glosses
                                        | value = str
                                        , valid = True
                                        , error = ""
                                        , changed = True
                                    }
                            }
                    }
                        |> validate
            in
            if String.isEmpty (String.trim str) then
                ( ok, Cmd.none )

            else
                case
                    ( newTokens == textTokens
                    , breakMismatch breaks.value str
                    )
                of
                    ( False, _ ) ->
                        let
                            err : String
                            err =
                                [ "The number of tokens is not equal."
                                , "The text has"
                                , String.fromInt textTokens ++ "."
                                , "This line has"
                                , String.fromInt newTokens ++ "."
                                ]
                                    |> String.join " "
                        in
                        ( validate
                            { model
                                | annotations =
                                    { annotations
                                        | glosses =
                                            { glosses
                                                | value = str
                                                , valid = False
                                                , error = err
                                                , changed = True
                                            }
                                    }
                            }
                        , Cmd.none
                        )

                    ( True, Just mismatch ) ->
                        let
                            err : String
                            err =
                                [ "There are incorrect affix breaks."
                                , "'" ++ mismatch.token1 ++ "' has"
                                , String.fromInt mismatch.length1 ++ "."
                                , "'" ++ mismatch.token2 ++ "' has"
                                , String.fromInt mismatch.length2 ++ "."
                                ]
                                    |> String.join " "
                        in
                        ( validate
                            { model
                                | annotations =
                                    { annotations
                                        | glosses =
                                            { glosses
                                                | value = str
                                                , valid = False
                                                , error = err
                                                , changed = True
                                            }
                                    }
                            }
                        , Cmd.none
                        )

                    ( True, Nothing ) ->
                        ( ok, Cmd.none )

        Phonemic str ->
            let
                newTokens : Int
                newTokens =
                    tokens str

                ok : Model
                ok =
                    { model
                        | annotations =
                            { annotations
                                | phonemic =
                                    { phonemic
                                        | value = str
                                        , valid = True
                                        , error = ""
                                        , changed = True
                                    }
                            }
                    }
                        |> validate
            in
            if String.isEmpty (String.trim str) then
                ( ok, Cmd.none )

            else if newTokens == textTokens then
                ( ok, Cmd.none )

            else
                let
                    err : String
                    err =
                        [ "The number of tokens is not equal."
                        , "The text has"
                        , String.fromInt textTokens ++ "."
                        , "This line has"
                        , String.fromInt newTokens ++ "."
                        ]
                            |> String.join " "
                in
                ( validate
                    { model
                        | annotations =
                            { annotations
                                | phonemic =
                                    { phonemic
                                        | value = str
                                        , valid = False
                                        , error = err
                                        , changed = True
                                    }
                            }
                    }
                , Cmd.none
                )

        Judgment str ->
            ( validate
                { model
                    | annotations =
                        { annotations
                            | judgment =
                                { judgment
                                    | value = str
                                    , valid = True
                                    , error = ""
                                    , changed = True
                                }
                        }
                }
            , Cmd.none
            )

        Trans str ->
            case String.toInt str of
                Nothing ->
                    if str == "add" then
                        let
                            counter : Int
                            counter =
                                model.counter + 1

                            trans : Translation
                            trans =
                                { id = counter
                                , deleted = False
                                , translation =
                                    { value = ""
                                    , valid = False
                                    , error = "Cannot be blank."
                                    , changed = True
                                    , original = ""
                                    }
                                , judgment =
                                    { value = ""
                                    , valid = True
                                    , error = ""
                                    , changed = False
                                    , original = ""
                                    }
                                }

                            ntranslations : List Translation
                            ntranslations =
                                translations
                                    |> List.reverse
                                    |> (::) trans
                                    |> List.reverse
                        in
                        ( validate { model | translations = ntranslations }
                        , Cmd.none
                        )

                    else
                        ( validate model, Cmd.none )

                Just id ->
                    case divided id of
                        Just ( prefix, translation, suffix ) ->
                            let
                                ntranslation : Translation
                                ntranslation =
                                    { translation
                                        | deleted = not translation.deleted
                                    }

                                ntranslations : List Translation
                                ntranslations =
                                    prefix ++ (ntranslation :: suffix)
                            in
                            ( validate { model
                                           | translations = ntranslations
                                       }
                            , Cmd.none
                            )

                        Nothing ->
                            ( validate model, Cmd.none )

        TTranslation id str ->
            case divided id of
                Just ( prefix, translation, suffix ) ->
                    let
                        ttranslation : StringField
                        ttranslation =
                            translation.translation

                        goodTranslation : Translation
                        goodTranslation =
                            { translation
                                | translation =
                                    { ttranslation
                                        | value = str
                                        , valid = True
                                        , error = ""
                                        , changed = True
                                    }
                            }

                        err : String
                        err =
                            [ "The translation cannot be blank."
                            , "If you want to remove the translation,"
                            , "you must delete it."
                            ]
                                |> String.join " "

                        badTranslation : Translation
                        badTranslation =
                            { translation
                                | translation =
                                    { ttranslation
                                        | value = str
                                        , valid = False
                                        , error = err
                                        , changed = True
                                    }
                            }

                        ntranslation : Translation
                        ntranslation =
                            if String.isEmpty (String.trim str) then
                                badTranslation

                            else
                                goodTranslation

                        ntranslations : List Translation
                        ntranslations =
                            prefix ++ (ntranslation :: suffix)
                    in
                    ( validate { model | translations = ntranslations }
                    , Cmd.none
                    )

                -- Unexpected, do nothing
                Nothing ->
                    ( model, Cmd.none )

        TJudgment id str ->
            case divided id of
                Just ( prefix, translation, suffix ) ->
                    let
                        tjudgment : StringField
                        tjudgment =
                            translation.judgment

                        ntranslation : Translation
                        ntranslation =
                            { translation
                                | judgment =
                                    { tjudgment
                                        | value = str
                                        , valid = True
                                        , error = ""
                                        , changed = True
                                    }
                            }

                        ntranslations : List Translation
                        ntranslations =
                            prefix ++ (ntranslation :: suffix)
                    in
                    ( validate { model | translations = ntranslations }
                    , Cmd.none
                    )

                -- Unexpected, do nothing
                Nothing ->
                    ( model, Cmd.none )

        Save ->
            if validate model |> .valid then
                ( { model | submitted = True }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Cancel ->
            -- Indicates that this is a new interlinear gloss. Set it
            -- to the default empty instance.
            if String.isEmpty text.original then
                ( initData model.id, Cmd.none )

            else
                let
                    renew : String -> StringField
                    renew orig =
                        { blankString | value = orig, original = orig }
                in
                ( { model
                    | changed = False
                    , submitted = False
                    , error = ""
                    , valid = True
                    , text = renew text.original
                    , annotations =
                        { breaks = renew breaks.original
                        , glosses = renew glosses.original
                        , phonemic = renew phonemic.original
                        , judgment = renew judgment.original
                        }
                    , translations =
                        List.filter
                            (\x ->
                                x.translation.original
                                    |> String.isEmpty
                                    |> not
                            )
                            translations
                            |> List.map
                                (\x ->
                                    { id = x.id
                                    , deleted = False
                                    , translation =
                                        renew x.translation.original
                                    , judgment =
                                        renew x.judgment.original
                                    }
                                )
                    , counter =
                        List.map (\x -> x.id) translations
                            |> List.maximum
                            |> Maybe.withDefault 0
                  }
                , Cmd.none
                )


{-| Display the form. The function f likely wraps types that are not
relevant to the form logic but that result in changes that are handled
in `change`
-}
view : Model -> Html.Html Msg
view model =
    Html.form []
        [ Html.fieldset []
            [ displayField
                { formname = "interlinear"
                , label = "Text"
                , kind = Html.textarea
                , oninput = Text
                , name = "text"
                , value = model.text.value
                , original = model.text.original
                , changed = model.text.changed
                , valid = model.text.valid
                , help = "The text to be glossed."
                , error = model.text.error
                , disabled = False
                , deleted = False
                , spellcheck = False
                , options = []
                , id = Nothing
                }
            ]
        , Html.fieldset []
            [ displayField
                { formname = "interlinear"
                , label = "Text Judgment"
                , kind = Html.input
                , oninput = Judgment
                , name = "judgement"
                , value = model.annotations.judgment.value
                , original = model.annotations.judgment.original
                , changed = model.annotations.judgment.changed
                , valid = model.annotations.judgment.valid
                , help = "Optional judgment, such as * or #."
                , error = model.annotations.judgment.error
                , disabled = False
                , deleted = False
                , spellcheck = False
                , options = []
                , id = Nothing
                }
            , displayField
                { formname = "interlinear"
                , label = "Phonemic Transcription"
                , kind = Html.textarea
                , oninput = Phonemic
                , name = "phonemic"
                , value = model.annotations.phonemic.value
                , original = model.annotations.phonemic.original
                , changed = model.annotations.phonemic.changed
                , valid = model.annotations.phonemic.valid
                , help = "Optional phonemic transcription."
                , error = model.annotations.phonemic.error
                , disabled = False
                , deleted = False
                , spellcheck = False
                , options = []
                , id = Nothing
                }
            , displayField
                { formname = "interlinear"
                , label = "Affix Breaks"
                , kind = Html.textarea
                , oninput = Breaks
                , name = "breaks"
                , value = model.annotations.breaks.value
                , original = model.annotations.breaks.original
                , changed = model.annotations.breaks.changed
                , valid = model.annotations.breaks.valid
                , help =
                    String.join " "
                        [ "Optional affix break annotation."
                        , "This is needed for glosses, below."
                        ]
                , error = model.annotations.breaks.error
                , disabled = False
                , deleted = False
                , spellcheck = False
                , options = []
                , id = Nothing
                }
            , displayField
                { formname = "interlinear"
                , label = "Affix Glosses"
                , kind = Html.textarea
                , oninput = Glosses
                , name = "glosses"
                , value = model.annotations.glosses.value
                , original = model.annotations.glosses.original
                , changed = model.annotations.glosses.changed
                , valid = model.annotations.glosses.valid
                , help = "Optional glosses."
                , error = model.annotations.glosses.error
                , disabled = False
                , deleted = False
                , spellcheck = False
                , options = []
                , id = Nothing
                }
            ]
        , Html.fieldset [] <|
            List.concat
                [ [ Html.legend [] [ Html.text "Translations" ] ]
                , List.map viewTrans model.translations
                , [ Html.a
                        [ Event.onClick <|
                            Trans "add"
                        , Attr.href "#"
                        ]
                        [ Html.text "+ Add Translation" ]
                  ]
                ]
        , Html.button
            (if model.valid then
                [ Event.onClick Save
                , Attr.type_ "button"
                ]

             else
                [ Attr.attribute "data-tooltip" model.error
                , Attr.attribute "data-placement" "right"
                , Attr.type_ "button"
                , Event.onClick None
                ]
            )
            [ Html.text "Save" ]
        , Html.button
            [ Attr.class "secondary"
            , Attr.attribute "data-tooltip" "This will erase all changes!"
            , Attr.attribute "data-placement" "right"
            , Attr.type_ "button"
            , Event.onClick Cancel
            ]
            [ Html.text "Cancel" ]
        ]


{-| A helper view function for translations
-}
viewTrans : Translation -> Html.Html Msg
viewTrans trans =
    Html.article []
        [ Html.header []
            [ Html.label []
                [ Html.input
                    [ Attr.type_ "checkbox"
                    , roleAttr "switch"
                    , Attr.checked trans.deleted
                    , Event.onCheck (\_ -> Trans (String.fromInt trans.id))
                    ]
                    []
                , Html.text "Remove Translation"
                ]
            ]
        , displayField
            { formname = "interlinear"
            , label = "Translation"
            , kind = Html.textarea
            , oninput = TTranslation trans.id
            , name = "translation"
            , value = trans.translation.value
            , original = trans.translation.original
            , changed = trans.translation.changed
            , valid = trans.translation.valid
            , help = "A translation of the text."
            , error = trans.translation.error
            , disabled = trans.deleted
            , deleted = trans.deleted
            , spellcheck = True
            , options = []
            , id = Just trans.id
            }
        , displayField
            { formname = "interlinear"
            , label = "Translation Judgment"
            , kind = Html.input
            , oninput = TJudgment trans.id
            , name = "judgment"
            , value = trans.judgment.value
            , original = trans.judgment.original
            , changed = trans.judgment.changed
            , valid = trans.judgment.valid
            , help = "Optional judgment, such as * or #."
            , error = trans.judgment.error
            , disabled = trans.deleted
            , deleted = trans.deleted
            , spellcheck = False
            , options = []
            , id = Just trans.id
            }
        ]


{-| An HTML attibute that isn't included in the standard Elm library.
-}
roleAttr : String -> Html.Attribute msg
roleAttr role =
    Attr.attribute "role" role
