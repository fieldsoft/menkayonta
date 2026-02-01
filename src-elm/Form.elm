module Form exposing
    ( CForm(..)
    , FieldId(..)
    , GlobalField(..)
    , GlobalFormData
    , ImportField(..)
    , ImportFormData
    , ProjectField(..)
    , ProjectFormData
    , globalFormData
    , importFormData
    , projectFormData
    )

import UUID
import Form.Interlinear
import Form.Shared exposing (StringField, SelectField, blankString, blankSelect)


type CForm
    = InterlinearCForm Form.Interlinear.Data
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


type FieldId
    = InterlinearForm Form.Interlinear.Field
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
