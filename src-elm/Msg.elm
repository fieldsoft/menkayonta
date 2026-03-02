module Msg exposing (Msg(..), ReceiveType(..), RequestType(..))

import Form.Project
import Json.Encode as E
import Menkayonta as M
import Meta
import UUID


type Msg
    = Received ReceiveType
    | Request ProjectId RequestType
    | NewInterlinear ProjectId
    | EditInterlinear ProjectId M.Interlinear
    | EditPerson ProjectId M.Person
    | NewProject
    | EditProject Form.Project.Model
    | UserClick Msg
    | ChangeTag (Maybe Meta.TagField)
    | SaveTag Meta.TagField
    | None


type alias ProjectId =
    UUID.UUID


type ReceiveType
    = IViewArea E.Value
    | IComposite E.Value
    | IGlobalConfig E.Value
    | IReversal E.Value
    | IInterlinearListing E.Value
    | IPersonListing E.Value
    | IReload E.Value


type RequestType
    = OReversal (Maybe String)
    | OInterlinearListing
    | OComposite String
    | OPersonListing
