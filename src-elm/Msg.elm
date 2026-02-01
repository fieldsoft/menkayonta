module Msg exposing (Msg(..), sendMsg)

import Content exposing (Content)
import Config exposing (ProjectInfo)
import Tab exposing (TabPath, Direction, Ventana)
import Task
import Time
import Form exposing (FieldId, CForm)
import Json.Encode as E


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
    | NewTab Ventana
    | None
    | ReceivedAllDoc E.Value
    | ReceivedDoc E.Value
    | ReceivedGlobalConfig E.Value
    | ReceivedInterlinearIndex E.Value
    | ReceivedPersonIndex E.Value
    | RequestAllDocId String String
    | RequestDocId String String
    | RequestInterlinearIndex String
    | RequestProjectIndex String
    | SetTime Time.Posix
    | SetWindowTitle String


sendMsg : Msg -> Cmd Msg
sendMsg msg =
    Task.succeed msg
        |> Task.perform identity
