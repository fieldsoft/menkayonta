module Form.Sequence exposing
    ( Item
    , Model
    , Msg(..)
    , init
    , initData
    , update
    , view
    )

import Dict
import Form.Shared
    exposing
        ( SelectField
        , StringField
        , blankSelect
        , blankString
        , displayField
        , displaySelectField
        )
import Html
import Html.Attributes as Attr
import Html.Events as Event
import List.Extra as LE
import Menkayonta
import UUID


{-| The Model describes a form representation of the Sequence type
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
    , title : StringField
    , description : StringField
    , kind : SelectField
    , items : List Item
    }


{-| The `Msg` type largely deals with the fields of the form. Buttons
are considered a field, as are groupings, where Itm represents the
group of items.
-}
type Msg
    = Title String
    | Description String
    | Kind String
    | AddItem
    | DeleteItem Int
    | IKey Int String
    | IValue Int String
    | Save
    | Cancel
    | None


{-| Items occur in a list in the form.
-}
type alias Item =
    { deleted : Bool
    , key : StringField
    , value : StringField
    }


{-| The initialization function. This will likely be refactored to
place more of the initialization logic in the function.
-}
init : Menkayonta.Sequence -> ( Model, Cmd Msg )
init seq =
    let
        item_ :
            Menkayonta.SequenceItem
            -> Item
        item_ { key, value } =
            { deleted = False
            , key =
                { value = key
                , valid = True
                , error = ""
                , changed = False
                , original = key
                }
            , value =
                { value = "interlinear/" ++ UUID.toString value
                , valid = True
                , error = ""
                , changed = False
                , original = "interlinear/" ++ UUID.toString value
                }
            }

        kindString : Menkayonta.SequenceKind -> String
        kindString k =
            case k of
                Menkayonta.Integer ->
                    "integer"

                Menkayonta.StringKey ->
                    "string"

        items : List Item
        items =
            seq.items
                |> List.map item_
    in
    ( { id = seq.id
      , rev = seq.rev
      , version = seq.version
      , changed = False
      , submitted = False
      , error = ""
      , valid = True
      , title =
            { value = seq.title
            , valid = True
            , error = ""
            , changed = False
            , original = seq.title
            }
      , description =
            { value = seq.description
            , valid = True
            , error = ""
            , changed = False
            , original = seq.description
            }
      , kind =
            { value = kindString seq.kind
            , options =
                [ ( "String"
                  , "string"
                  )
                , ( "Integer"
                  , "integer"
                  )
                ]
            , valid = True
            , changed = False
            , error = ""
            , original = kindString seq.kind
            }
      , items = items
      }
    , Cmd.none
    )


kindOpts : List ( String, String )
kindOpts =
    [ ( "String"
      , "string"
      )
    , ( "Integer"
      , "integer"
      )
    ]


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
    , title = { blankString | valid = False, error = "Cannot be empty." }
    , description = blankString
    , kind =
        { blankSelect
            | options = kindOpts
        }
    , items = []
    }


{-| This handles events specific to particular fields.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        title : StringField
        title =
            model.title

        description : StringField
        description =
            model.description

        items : List Item
        items =
            model.items

        kind : SelectField
        kind =
            model.kind

        -- Return false if there are any invalid translation items.
        itemsValid : List Item -> Bool
        itemsValid items_ =
            items_
                |> List.concatMap
                    (\x ->
                        -- The translations are not required. So, any
                        -- deleted translation is valid.
                        if x.deleted then
                            [ True ]

                        else
                            [ x.key.valid, x.value.valid ]
                    )
                |> List.all identity

        -- Return false if there are any invalid items
        valid : Model -> Bool
        valid d_ =
            List.all identity
                [ d_.title.valid
                , d_.description.valid
                , d_.kind.valid
                , itemsValid d_.items
                ]

        toperr : String
        toperr =
            "Correct errors before saving."

        -- Set top level validity and error messages accordingly.
        validate : Model -> Model
        validate model_ =
            { model_
                | changed = True
                , valid = valid model_
                , error =
                    if valid model_ then
                        ""

                    else
                        toperr
            }
    in
    case msg of
        -- It is sometimes useful to have a non-operation.
        None ->
            ( model, Cmd.none )

        Title str ->
            if String.isEmpty str then
                ( validate
                    { model
                        | title =
                            { title
                                | value = str
                                , valid = False
                                , error = "The title can't be blank."
                                , changed = True
                            }
                    }
                , Cmd.none
                )

            else
                ( validate
                    { model
                        | title =
                            { title
                                | value = str
                                , valid = True
                                , error = ""
                                , changed = True
                            }
                    }
                , Cmd.none
                )

        Description str ->
            ( validate
                { model
                    | description =
                        { description
                            | value = str
                            , valid = True
                            , error = ""
                            , changed = True
                        }
                }
            , Cmd.none
            )

        Kind str ->
            let
                withNewKind : SelectField -> Model
                withNewKind k =
                    { model | kind = k }

                checkItems : SelectField -> Model
                checkItems k =
                    LE.indexedFoldl
                        (\index item m0 ->
                            let
                                ( m1, _ ) =
                                    update (IKey index item.key.value) m0
                            in
                            m1
                        )
                        (withNewKind k)
                        items
            in
            if String.isEmpty str then
                ( { kind
                    | value = str
                    , valid = False
                    , error = "The title can't be blank."
                    , changed = True
                  }
                    |> checkItems
                    |> validate
                , Cmd.none
                )

            else
                ( { kind
                    | value = str
                    , valid = True
                    , error = ""
                    , changed = True
                  }
                    |> checkItems
                    |> validate
                , Cmd.none
                )

        DeleteItem index ->
            case LE.getAt index items of
                Just item ->
                    let
                        item_ : Item
                        item_ =
                            { item | deleted = not item.deleted }

                        items_ : List Item
                        items_ =
                            LE.setAt index item_ items
                    in
                    ( validate { model | items = items_ }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )
                    
        AddItem ->
            let
                item : Item
                item =
                    { deleted = False
                    , key =
                          { value = ""
                          , valid = False
                          , error = "Cannot be blank."
                          , changed = True
                          , original = ""
                          }
                    , value =
                          { value = ""
                          , valid = True
                          , error = ""
                          , changed = False
                          , original = ""
                          }
                    }

                nitems : List Item
                nitems =
                    item :: items
            in
            ( validate { model | items = nitems }
            , Cmd.none
            )

        IKey index str ->
            case LE.getAt index items of
                Nothing ->
                    ( model, Cmd.none )
                        
                Just item ->
                    let
                        key : StringField
                        key =
                            item.key

                        itemErr : String -> Item
                        itemErr err_ =
                            { item
                                | key =
                                    { key
                                        | value = str
                                        , valid = False
                                        , error = err_
                                        , changed = True
                                    }
                            }

                        itemGood : Item
                        itemGood =
                            { item
                                | key =
                                    { key
                                        | value = str
                                        , valid = True
                                        , error = ""
                                        , changed = True
                                    }
                            }

                        itemsGood : List Item
                        itemsGood =
                            LE.setAt index itemGood items

                        itemsError : String -> List Item
                        itemsError err_ =
                            LE.setAt index (itemErr err_) items

                        nmodel : List Item -> Model
                        nmodel is =
                            validate { model | items = is }
                    in
                    if String.isEmpty str then
                        let
                            err : String
                            err =
                                [ "The key cannot be blank. "
                                , "If you want to remove the item, "
                                , "you must delete it."
                                ]
                                    |> String.concat

                            items_ : List Item
                            items_ =
                                itemsError err
                        in
                        ( nmodel items_
                        , Cmd.none
                        )

                    else
                        case model.kind.value of
                            "integer" ->
                                case String.toInt str of
                                    Just _ ->
                                        ( nmodel itemsGood
                                        , Cmd.none
                                        )

                                    Nothing ->
                                        let
                                            err : String
                                            err =
                                                [ "The sequence kind is set to integer, "
                                                , "but the current key for this items is "
                                                , "not an integer. Change it to a "
                                                , "non-decimal number value, or change "
                                                , "the type of the sequence to \"string\""
                                                ]
                                                    |> String.concat

                                            items_ : List Item
                                            items_ =
                                                itemsError err
                                        in
                                        ( nmodel items_
                                        , Cmd.none
                                        )

                            "string" ->
                                ( nmodel itemsGood
                                , Cmd.none
                                )

                            _ ->
                                let
                                    err : String
                                    err =
                                        [ "The sequence kind is not properly set. "
                                        , "Choose an option for the \"Kind\" field."
                                        ]
                                            |> String.concat

                                    items_ : List Item
                                    items_ =
                                        itemsError err
                                in
                                ( nmodel items_
                                , Cmd.none
                                )

        IValue index str ->
            case LE.getAt index items of
                Nothing ->
                    ( model, Cmd.none )
                        
                Just item ->
                    let
                        value : StringField
                        value =
                            item.value

                        itemErr : String -> Item
                        itemErr err_ =
                            { item
                                | value =
                                    { value
                                        | value = str
                                        , valid = False
                                        , error = err_
                                        , changed = True
                                    }
                            }

                        itemGood : Item
                        itemGood =
                            { item
                                | value =
                                    { value
                                        | value = str
                                        , valid = True
                                        , error = ""
                                        , changed = True
                                    }
                            }

                        itemsGood : List Item
                        itemsGood =
                            LE.setAt index itemGood items

                        itemsError : String -> List Item
                        itemsError err_ =
                            LE.setAt index (itemErr err_) items

                        nmodel : List Item -> Model
                        nmodel is =
                            validate { model | items = is }
                    in
                    if String.isEmpty str then
                        let
                            err : String
                            err =
                                [ "The value cannot be blank. "
                                , "If you want to remove the item, "
                                , "you must delete it."
                                ]
                                    |> String.concat

                            items_ : List Item
                            items_ =
                                itemsError err
                        in
                        ( nmodel items_
                        , Cmd.none
                        )

                    else
                        let
                            splitVal : List String
                            splitVal =
                                String.split "/" str

                            isInterlinear : Bool
                            isInterlinear =
                                case splitVal of
                                    "interlinear" :: uuid :: [] ->
                                        case UUID.fromString uuid of
                                            Ok _ ->
                                                True

                                            _ ->
                                                False

                                    _ ->
                                        False
                        in
                        if isInterlinear then
                            ( nmodel itemsGood
                            , Cmd.none
                            )

                        else
                            let
                                err : String
                                err =
                                    [ "The identifier for the interlinear gloss is "
                                    , "malformed. Try copying and pasting."
                                    ]
                                    |> String.concat

                                items_ : List Item
                                items_ =
                                    itemsError err
                            in
                            ( nmodel items_
                            , Cmd.none
                            )

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
            if String.isEmpty title.original then
                ( initData model.id, Cmd.none )

            else
                let
                    renew : String -> StringField
                    renew orig =
                        { blankString | value = orig, original = orig }

                    renewKind : String -> SelectField
                    renewKind orig =
                        { blankSelect
                            | value = orig
                            , original = orig
                            , options = kindOpts
                        }
                in
                ( { model
                    | changed = False
                    , submitted = False
                    , error = ""
                    , valid = True
                    , title = renew title.original
                    , description = renew description.original
                    , kind = renewKind kind.original
                    , items =
                        List.filter
                            (\x ->
                                x.key.original
                                    |> String.isEmpty
                                    |> not
                            )
                            items
                            |> List.map
                                (\x ->
                                    { deleted = False
                                    , key =
                                        renew x.key.original
                                    , value =
                                        renew x.value.original
                                    }
                                )
                  }
                , Cmd.none
                )


{-| Display the form. -}
view : Model -> Html.Html Msg
view model =
    Html.form []
        [ Html.fieldset []
            [ displayField
                { formname = "sequence"
                , label = "Title"
                , kind = Html.input
                , oninput = Title
                , name = "title"
                , value = model.title.value
                , original = model.title.original
                , changed = model.title.changed
                , valid = model.title.valid
                , help = "The title of the sequence."
                , error = model.title.error
                , disabled = False
                , deleted = False
                , spellcheck = True
                , options = []
                , id = Nothing
                }
            ]
        , Html.fieldset []
            [ displayField
                { formname = "sequence"
                , label = "Description"
                , kind = Html.textarea
                , oninput = Description
                , name = "description"
                , value = model.description.value
                , original = model.description.original
                , changed = model.description.changed
                , valid = model.description.valid
                , help = "A brief description of the sequence."
                , error = model.description.error
                , disabled = False
                , deleted = False
                , spellcheck = True
                , options = []
                , id = Nothing
                }
            , displaySelectField
                { formname = "sequence"
                , label = "Sequence Kind"
                , kind = Html.select
                , oninput = Kind
                , name = "kind"
                , value = model.kind.value
                , original = model.kind.original
                , changed = model.kind.changed
                , valid = model.kind.valid
                , help = "If you choose integer, you must enter valid number keys. If you choose string, your keys may be of any form but 22 will sort before 3 based on alphabetic order. Padding by zero may help if you wish to mix numbers and letters. For instance 003 and 022 sort correctly."
                , error = model.kind.error
                , disabled = False
                , deleted = False
                , spellcheck = False
                , options = model.kind.options
                , id = Nothing
                }
            ]
        , Html.fieldset [] <|
            List.concat
                [ [ Html.legend [] [ Html.text "Items" ] ]
                , List.indexedMap viewItem model.items
                , [ Html.a
                        [ Event.onClick <|
                            AddItem
                        , Attr.href "#"
                        ]
                        [ Html.text "+ Add Item" ]
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
viewItem : Int -> Item -> Html.Html Msg
viewItem index item =
    Html.article []
        [ Html.header []
            [ Html.label []
                [ Html.input
                    [ Attr.type_ "checkbox"
                    , roleAttr "switch"
                    , Attr.checked item.deleted
                    , Event.onCheck (\_ -> DeleteItem index)
                    ]
                    []
                , Html.text "Remove Item"
                ]
            ]
        , displayField
            { formname = "sequence"
            , label = "Key"
            , kind = Html.input
            , oninput = IKey index
            , name = "key"
            , value = item.key.value
            , original = item.key.original
            , changed = item.key.changed
            , valid = item.key.valid
            , help = "A key to sort by."
            , error = item.key.error
            , disabled = item.deleted
            , deleted = item.deleted
            , spellcheck = False
            , options = []
            , id = Just index
            }
        , displayField
            { formname = "sequence"
            , label = "Interlinear Identifier"
            , kind = Html.input
            , oninput = IValue index
            , name = "value"
            , value = item.value.value
            , original = item.value.original
            , changed = item.value.changed
            , valid = item.value.valid
            , help = "This needs to be a valid identifier for an interlinear gloss."
            , error = item.value.error
            , disabled = item.deleted
            , deleted = item.deleted
            , spellcheck = False
            , options = []
            , id = Just index
            }
        ]


{-| An HTML attibute that isn't included in the standard Elm library.
-}
roleAttr : String -> Html.Attribute msg
roleAttr role =
    Attr.attribute "role" role
