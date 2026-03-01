module Msg exposing (Msg(..), RequestType(..), ReceiveType(..))

import UUID
import Json.Encode as E
import Menkayonta as M
import Form.Project

type Msg
    = Received ReceiveType
    | Request ProjectId RequestType
    | NewInterlinear ProjectId
    | EditInterlinear ProjectId M.Interlinear
    | EditPerson ProjectId M.Person
    | NewProject
    | EditProject Form.Project.Model
    | UserClick Msg

type alias ProjectId =
    UUID.UUID


type ReceiveType
    = IViewArea E.Value
    | IComposite E.Value
    | IGlobalConfig E.Value
    | IReversal E.Value
    | IInterlinearListing E.Value
    | IPersonListing E.Value


type RequestType
    = OReversal (Maybe String)
    | OInterlinearListing
    | OComposite String
    | OPersonListing

        
