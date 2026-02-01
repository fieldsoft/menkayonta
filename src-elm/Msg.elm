module Msg exposing (Msg(..), sendMsg)

import Content exposing (Content)
import Config exposing (ProjectInfo)
import Tab exposing (Direction, Ventana)
import Task
import Time
import Form exposing (Field, CForm)
import Json.Encode as E


type Msg
    = ChangeEditParam Tab.Path
    | ChangeLengthParam Tab.Path String
    | ChangeSearchParam Tab.Path String
    | Tab Tab.Msg
    | FormChange Tab.Path Field String
    | FormInit String CForm
    | FormSubmit Tab.Path
    | GlobalSettingsMenu E.Value
    | ImportOptionsFileMenu String
    | MultiMsg (List Msg)
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
