module Msg exposing (Msg(..), FieldKind(..), FormData, sendMsg)

import Content exposing (Content)
import Config exposing (ProjectInfo)
import Tab exposing (TabPath, Direction, Ventana)
import Task
import Time
import Content.Form exposing (FieldId, CForm)
import Json.Encode as E
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Error as FError


type Msg
    = ChangeEditParam TabPath
    | ChangeLengthParam TabPath String
    | ChangeSearchParam TabPath String
    | CloneTab
    | CloseTab
    | FocusTab TabPath
    | FormChange TabPath FieldId String
    | FormInit String CForm
    | FormSubmit TabPath
    | GlobalSettingsMenu E.Value
    | GotoTab TabPath
    | ImportOptionsFileMenu String
    | Move Direction
    | MultiMsg (List Msg)
    | NewProjectMenu String
    | NewTab Ventana
    | None
    | ProjectInfoFormChange TabPath (Field.Msg FieldKind)
    | ProjectSettingsEdit ProjectInfo
    | ReceivedAllDoc E.Value
    | ReceivedDoc E.Value
    | ReceivedGlobalConfig E.Value
    | ReceivedInterlinearIndex E.Value
    | ReceivedPersonIndex E.Value
    | ReceivedProjectIndex E.Value
    | RequestAllDocId String String
    | RequestDocId String String
    | RequestInterlinearIndex String
    | RequestProjectIndex String
    | SetTime Time.Posix
    | SetWindowTitle String


type alias FormData =
    { fields : Field FieldKind
    , submitted : Bool
    , result : Maybe (Result (FError.Error FieldKind) Content)
    }


type FieldKind
    = ProjectIdentifier
    | ProjectTitle
    | ProjectEnabled
    | ProjectUrl
    | GlobalName
    | GlobalEmail
    | GlobalUUID


sendMsg : Msg -> Cmd Msg
sendMsg msg =
    Task.succeed msg
        |> Task.perform identity
