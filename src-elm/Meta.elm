module Meta exposing (Model, TagField, Dialog)

import Set exposing (Set)
import Dict exposing (Dict)
import Menkayonta exposing (DocId(..))


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
    , docid : DocId
    }


type alias Dialog =
    { tag : Maybe TagField }
