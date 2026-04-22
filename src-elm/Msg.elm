module Msg exposing
    ( Msg(..)
    , ReceiveType(..)
    , RequestType(..)
    , EditType(..)
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
    | Edit EditType
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
    | ISequence E.Value
    | IPersonListing E.Value
    | IReload E.Value
    | INoteFor E.Value
    | INote E.Value


type RequestType
    = OReversal (Maybe String)
    | OInterlinearListing
    | OSequenceListing
    | OSequence String
    | OComposite String
    | OPersonListing
    | ODelete Revision String
    | ONoteFor M.GenericDesc
    | ONote String


type EditType
    = ESequence ProjectId M.Sequence
    | EInterlinear ProjectId M.Interlinear
    | EPerson ProjectId M.Person
    | NInterlinear ProjectId
    | NSequence ProjectId
    | NPerson ProjectId
      
