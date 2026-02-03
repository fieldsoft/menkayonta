module Form exposing
    ( CForm(..)
    , Field(..)
    , ImportField(..)
    , ImportFormData
    , ProjectField(..)
    , ProjectFormData
    , importFormData
    , projectFormData
    )

import Form.Global
import Form.Interlinear
import Form.Shared
    exposing
        ( SelectField
        , StringField
        , blankSelect
        , blankString
        )
import UUID
import Json.Encode as E

      
type CForm
    = ImportCForm ImportFormData
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


type Field
    = ImportForm ImportField
    | ProjectForm ProjectField


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


projectFormData =
    { changed = False
    , submitted = False
    , error = "Please fill the empty form."
    , valid = False
    , identifier = ""
    , title = { blankString | valid = False, error = "Cannot be empty." }
    , url = blankString
    }


type alias ImportOptions =
    { filepath : String
    , kind : String
    , project : String
    }
