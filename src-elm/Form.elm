module Form exposing
    ( CForm(..)
    , Field(..)
    , ImportField(..)
    , ImportFormData
    , importFormData
    )

import UUID
import Json.Encode as E
import Form.Shared
    exposing
        ( SelectField
        , blankSelect
        )

      
type CForm
    = ImportCForm ImportFormData


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


type alias ImportOptions =
    { filepath : String
    , kind : String
    , project : String
    }
