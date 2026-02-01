module Content.Form exposing
    ( CForm(..)
    , FieldId(..)
    , GlobalField(..)
    , GlobalFormData
    , ImportField(..)
    , ImportFormData
    , InterlinearAnnotationsData
    , InterlinearField(..)
    , InterlinearFormData
    , InterlinearTranslationData
    , ProjectField(..)
    , ProjectFormData
    , blankString
    , globalFormData
    , importFormData
    , interlinearFormData
    , projectFormData
    , blankSelect
    )

import UUID


type CForm
    = InterlinearCForm InterlinearFormData
    | ImportCForm ImportFormData
    | GlobalCForm GlobalFormData
    | ProjectCForm ProjectFormData


type alias ProjectFormData =
    { changed : Bool
    , submitted : Bool
    , error : String
    , valid : Bool
    , identifier : String
    , title : StringField
    , url : StringField
    }
    

type alias InterlinearFormData =
    { id : Maybe UUID.UUID
    , rev : Maybe String
    , version : Int
    , changed : Bool
    , submitted : Bool
    , error : String
    , valid : Bool
    , text : StringField
    , annotations : InterlinearAnnotationsData
    , translations : List InterlinearTranslationData
    , counter : Int
    }


type alias ImportFormData =
    { changed : Bool
    , submitted : Bool
    , error : String
    , valid : Bool
    , filepath : String
    , kind : SelectField
    , project : SelectField
    }


type alias GlobalFormData =
    { changed : Bool
    , submitted : Bool
    , error : String
    , valid : Bool
    , name : StringField
    , email : StringField
    }


type alias StringField =
    { value : String
    , valid : Bool
    , error : String
    , changed : Bool
    , original : String
    }


type alias SelectField =
    { value : String
    , options : List ( String, String )
    , valid : Bool
    , error : String
    , changed : Bool
    , original : String
    }


type alias InterlinearAnnotationsData =
    { breaks : StringField
    , glosses : StringField
    , phonemic : StringField
    , judgment : StringField
    }


type alias InterlinearTranslationData =
    { id : Int
    , deleted : Bool
    , translation : StringField
    , judgment : StringField
    }


type FieldId
    = InterlinearForm InterlinearField
    | ImportForm ImportField
    | GlobalForm GlobalField
    | ProjectForm ProjectField


type GlobalField
    = GlbEmailF
    | GlbNameF
    | GlbSaveF
    | GlbCancelF


type ProjectField
    = PrjTitleF
    | PrjUrlF
    | PrjSaveF
    | PrjCancelF


type ImportField
    = ImpKindF
    | ImpProjectF
    | ImpImportF
    | ImpCancelF


type InterlinearField
    = IntTextF
    | IntBreaksF
    | IntGlossesF
    | IntPhonemicF
    | IntJudgmentF
    | IntTransF
    | IntTransTranslationF Int
    | IntTransJudgmentF Int
    | IntSaveF
    | IntCancelF


blankString : StringField
blankString =
    { value = ""
    , valid = True
    , error = ""
    , changed = False
    , original = ""
    }


blankSelect : SelectField
blankSelect =
    { value = ""
    , options = []
    , valid = True
    , error = ""
    , changed = False
    , original = ""
    }


interlinearFormData : InterlinearFormData
interlinearFormData =
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


importFormData =
    { changed = False
    , submitted = False
    , error = "Please fill the empty form."
    , valid = False
    , filepath = ""
    , kind =
        { blankSelect
            | options =
                [ ( "Dative Form Json"
                  , "Dative Form Json"
                  )
                ]
        }
    , project = blankSelect
    }


globalFormData =
    { changed = False
    , submitted = False
    , error = "Please fill the empty form."
    , valid = False
    , email = { blankString | valid = False, error = "Cannot be empty." }
    , name = { blankString | valid = False, error = "Cannot be empty." }
    }


projectFormData =
    { changed = False
    , submitted = False
    , error = "Please fill the empty form."
    , valid = False
    , identifier = ""
    , title = { blankString | valid = False, error = "Cannot be empty." }
    , url = blankString
    }
