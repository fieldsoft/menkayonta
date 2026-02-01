module Form.Interlinear exposing
    ( Annotations
    , Data
    , Field
    , Translation
    , change
    , display
    , init
    , initData
    )

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
import Dict


{-| The Data type alias describes a form representation of the
Interlinear type from Menkayonta. -}
type alias Data =
    { id : Maybe UUID.UUID
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
    

{-| These are the fields of the form. Buttons are considered a field,
as are groupings, where Trans represents the group of translations. -}
type Field
    = Text
    | Breaks
    | Glosses
    | Phonemic
    | Judgment
    | Trans
    | TTranslation Int
    | TJudgment Int
    | Save
    | Cancel
    | None
      

{-| Annotations are the non-translation annotations. -}
type alias Annotations =
    { breaks : StringField
    , glosses : StringField
    , phonemic : StringField
    , judgment : StringField
    }


{-| Translations occur in a list (actually a dictionary). -}
type alias Translation =
    { id : Int
    , deleted : Bool
    , translation : StringField
    , judgment : StringField
    }


{-| The initialization function. This will likely be refactored to
place more of the initialization logic in the function. -}
init : Menkayonta.Interlinear -> Data
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

                        formData : Data
                        formData =
                            { id = Just int.id
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
    formData

{-| A starter Data instance. -}
initData : Data
initData =
    { id = Nothing
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


{-| This handles events specific to particular fields. -}
change : Field -> String -> Data -> Data
change fid str d =
    let
        tokens s =
            String.words s
                -- Don't include the empty string
                |> List.filter (\x -> String.length x > 0)
                |> List.length

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
                brx1 =
                    breax s1

                brx2 =
                    breax s2

                idx =
                    List.map2 (\a1 a2 -> a1 == a2) brx1 brx2
                        |> LE.findIndex not

                token1 =
                    Maybe.andThen (\i -> LE.getAt i (String.words s1)) idx

                token2 =
                    Maybe.andThen (\i -> LE.getAt i (String.words s2)) idx

                length1 =
                    Maybe.andThen (\i -> LE.getAt i brx1) idx

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

        str_ =
            String.trim str

        text =
            d.text

        annotations =
            d.annotations

        translations =
            d.translations

        judgment =
            annotations.judgment

        breaks =
            annotations.breaks

        glosses =
            annotations.glosses

        phonemic =
            annotations.phonemic

        translationsValid translations_ =
            translations_
                |> List.map
                    (\x -> [ x.translation.valid, x.judgment.valid ])
                |> List.concat
                |> List.all identity

        valid d_ =
            List.all identity
                [ d_.text.valid
                , d_.annotations.judgment.valid
                , d_.annotations.breaks.valid
                , d_.annotations.glosses.valid
                , d_.annotations.phonemic.valid
                , translationsValid d_.translations
                ]

        toperr =
            "Correct errors before saving."

        defD d_ =
            let
                valid_ =
                    valid d_
            in
            { d_
                | changed = True
                , valid = valid_
                , error =
                    if valid_ then
                        ""

                    else
                        toperr
            }

        -- The reason for dividing the list in this way is
        -- ensuring that the user doesn't have the elements of
        -- the user interface jump around while editing. It
        -- will also preserve the order from the Dict the
        -- items were derived from.
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

        textTokens =
            tokens d.text.value
    in
    case fid of
        -- It is sometimes useful to have a non-operation.
        None ->
            d

        Text ->
            if String.isEmpty str then
                { d
                    | text =
                        { text
                            | value = str
                            , valid = False
                            , error = "The text can't be blank."
                            , changed = True
                        }
                }
                    |> defD

            else
                { d
                    | text =
                        { text
                            | value = str
                            , valid = True
                            , error = ""
                            , changed = True
                        }
                }
                    |> defD

        Breaks ->
            let
                newTokens =
                    tokens str

                ok =
                    { d
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
                        |> defD
            in
            if String.isEmpty (String.trim str) then
                ok

            else if newTokens == textTokens then
                ok

            else
                let
                    err =
                        [ "The number of tokens is not equal."
                        , "The text has"
                        , String.fromInt textTokens ++ "."
                        , "This line has"
                        , String.fromInt newTokens ++ "."
                        ]
                            |> String.join " "
                in
                { d
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
                    |> defD

        Glosses ->
            let
                newTokens =
                    tokens str

                ok =
                    { d
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
                        |> defD
            in
            if String.isEmpty (String.trim str) then
                ok

            else
                case
                    ( newTokens == textTokens
                    , breakMismatch breaks.value str
                    )
                of
                    ( False, _ ) ->
                        let
                            err =
                                [ "The number of tokens is not equal."
                                , "The text has"
                                , String.fromInt textTokens ++ "."
                                , "This line has"
                                , String.fromInt newTokens ++ "."
                                ]
                                    |> String.join " "
                        in
                        { d
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
                            |> defD

                    ( True, Just mismatch ) ->
                        let
                            err =
                                [ "There are incorrect affix breaks."
                                , "'" ++ mismatch.token1 ++ "' has"
                                , String.fromInt mismatch.length1 ++ "."
                                , "'" ++ mismatch.token2 ++ "' has"
                                , String.fromInt mismatch.length2 ++ "."
                                ]
                                    |> String.join " "
                        in
                        { d
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
                            |> defD

                    ( True, Nothing ) ->
                        ok

        Phonemic ->
            let
                newTokens =
                    tokens str

                ok =
                    { d
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
                        |> defD
            in
            if String.isEmpty (String.trim str) then
                ok

            else if newTokens == textTokens then
                ok

            else
                let
                    err =
                        [ "The number of tokens is not equal."
                        , "The text has"
                        , String.fromInt textTokens ++ "."
                        , "This line has"
                        , String.fromInt newTokens ++ "."
                        ]
                            |> String.join " "
                in
                { d
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
                    |> defD

        Judgment ->
            { d
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
                |> defD

        Trans ->
            case String.toInt str of
                Nothing ->
                    if str == "add" then
                        let
                            counter =
                                d.counter + 1

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

                            ntranslations =
                                translations
                                    |> List.reverse
                                    |> (::) trans
                                    |> List.reverse
                        in
                        { d | translations = ntranslations }
                            |> defD

                    else
                        d |> defD

                Just id ->
                    case divided id of
                        Just ( prefix, translation, suffix ) ->
                            let
                                ntranslation =
                                    { translation
                                        | deleted = not translation.deleted
                                    }

                                ntranslations =
                                    prefix ++ (ntranslation :: suffix)
                            in
                            { d | translations = ntranslations }
                                |> defD

                        Nothing ->
                            d |> defD

        TTranslation id ->
            case divided id of
                Just ( prefix, translation, suffix ) ->
                    let
                        ttranslation =
                            translation.translation

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

                        err =
                            [ "The translation cannot be blank."
                            , "If you want to remove the translation,"
                            , "you must delete it."
                            ]
                                |> String.join " "

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

                        ntranslation =
                            if String.isEmpty (String.trim str) then
                                badTranslation

                            else
                                goodTranslation

                        ntranslations =
                            prefix ++ (ntranslation :: suffix)
                    in
                    { d | translations = ntranslations } |> defD

                -- Unexpected, do nothing
                Nothing ->
                    d

        TJudgment id ->
            case divided id of
                Just ( prefix, translation, suffix ) ->
                    let
                        tjudgment =
                            translation.judgment

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

                        ntranslations =
                            prefix ++ (ntranslation :: suffix)
                    in
                    { d | translations = ntranslations }
                        |> defD

                -- Unexpected, do nothing
                Nothing ->
                    d

        Save ->
            if defD d |> .valid then
                { d | submitted = True }

            else
                d

        Cancel ->
            -- Indicates that this is a new item
            if String.isEmpty text.original then
                initData

            else
                let
                    renew orig =
                        { blankString | value = orig, original = orig }
                in
                { d
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


{-| Display the form. The function f likely wraps types that are not
relevant to the form logic but that result in changes that are handled
in `change` -}
display : Data -> (Field -> String -> msg) -> Html.Html msg
display d f =
    Html.form []
        [ Html.fieldset []
            [ displayField
                { formname = "interlinear"
                , label = "Text"
                , kind = Html.textarea
                , oninput = f Text
                , name = "text"
                , value = d.text.value
                , original = d.text.original
                , changed = d.text.changed
                , valid = d.text.valid
                , help = "The text to be glossed."
                , error = d.text.error
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
                , oninput = f Judgment
                , name = "judgement"
                , value = d.annotations.judgment.value
                , original = d.annotations.judgment.original
                , changed = d.annotations.judgment.changed
                , valid = d.annotations.judgment.valid
                , help = "Optional judgment, such as * or #."
                , error = d.annotations.judgment.error
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
                , oninput = f Phonemic
                , name = "phonemic"
                , value = d.annotations.phonemic.value
                , original = d.annotations.phonemic.original
                , changed = d.annotations.phonemic.changed
                , valid = d.annotations.phonemic.valid
                , help = "Optional phonemic transcription."
                , error = d.annotations.phonemic.error
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
                , oninput = f Breaks
                , name = "breaks"
                , value = d.annotations.breaks.value
                , original = d.annotations.breaks.original
                , changed = d.annotations.breaks.changed
                , valid = d.annotations.breaks.valid
                , help =
                    String.join " "
                        [ "Optional affix break annotation."
                        , "This is needed for glosses, below."
                        ]
                , error = d.annotations.breaks.error
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
                , oninput = f Glosses
                , name = "glosses"
                , value = d.annotations.glosses.value
                , original = d.annotations.glosses.original
                , changed = d.annotations.glosses.changed
                , valid = d.annotations.glosses.valid
                , help = "Optional glosses."
                , error = d.annotations.glosses.error
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
                , List.map (displayTrans f) d.translations
                , [ Html.a
                        [ Event.onClick <|
                            f Trans "add"
                        , Attr.href "#"
                        ]
                        [ Html.text "+ Add Translation" ]
                  ]
                ]
        , Html.button
            (if d.valid then
                [ Event.onClick <|
                    f Save ""
                , Attr.type_ "button"
                ]

             else
                [ Attr.attribute "data-tooltip" d.error
                , Attr.attribute "data-placement" "right"
                , Attr.type_ "button"
                , Event.onClick <| f None ""
                ]
            )
            [ Html.text "Save" ]
        , Html.button
            [ Attr.class "secondary"
            , Attr.attribute "data-tooltip" "This will erase all changes!"
            , Attr.attribute "data-placement" "right"
            , Attr.type_ "button"
            , Event.onClick <| f Cancel ""
            ]
            [ Html.text "Cancel" ]
        ]


{-| A helper display function for translations -}
displayTrans : (Field -> String -> msg) -> Translation -> Html.Html msg
displayTrans f trans =
    Html.article []
        [ Html.header []
            [ Html.label []
                [ Html.input
                    [ Attr.type_ "checkbox"
                    , roleAttr "switch"
                    , Attr.checked trans.deleted
                    , Event.onCheck (\_ -> f Trans (String.fromInt trans.id))
                    ]
                    []
                , Html.text "Remove Translation"
                ]
            ]
        , displayField
            { formname = "interlinear"
            , label = "Translation"
            , kind = Html.textarea
            , oninput = f (TTranslation trans.id)
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
            , oninput = f (TJudgment trans.id)
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
