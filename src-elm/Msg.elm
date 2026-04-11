module Msg exposing
    ( Msg(..)
    , ReceiveType(..)
    , RequestType(..)
    , Revision
    , ProjectId
    )

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
    | EditSequence ProjectId M.Sequence
    | EditPerson ProjectId M.Person
    | NewProject
    | EditProject Form.Project.Model
    | UserClick Msg
    | ChangeTag (Maybe Meta.TagField)
    | ChangeNote String String
    | SaveTag Meta.TagField
    | SaveNote M.Note
    | ChangeProperty (Maybe Meta.PropertyField)
    | SaveProperty Meta.PropertyField
    | EditToggle
    | Status E.Value
    | None


type alias ProjectId =
    UUID.UUID


type alias Revision =
    Maybe String


type ReceiveType
    = IViewArea E.Value
    | IComposite E.Value
    | IGlobalConfig E.Value
    | IReversal E.Value
    | IInterlinearListing E.Value
    | ISequenceListing E.Value
    | IPersonListing E.Value
    | IReload E.Value
    | INoteFor E.Value
    | INote E.Value


type RequestType
    = OReversal (Maybe String)
    | OInterlinearListing
    | OSequenceListing
    | OComposite String
    | OPersonListing
    | ODelete Revision String
    | ONoteFor M.Identifier M.Value
    | ONote String
