module Meta exposing (Dialog, Model, TagField)

import Dict exposing (Dict)
import Menkayonta exposing (DocId(..))
import Set exposing (Set)
import UUID


type alias Model =
    { tags : Set String
    , descriptions : Set String
    , properties : Dict String (List String)
    , modifications : Set String
    , people : Dict String (List String)
    , speakers : Dict String (List String)
    }


type alias TagField =
    { value : String
    , docids : List DocId
    , project : UUID.UUID
    }


type alias Dialog =
    { tag : Maybe TagField }
