module Content exposing (Content(..), translationsDecoder, Translation)

import Menkayonta exposing (Interlinear, OneDoc)
import Content.Form exposing (CForm)
import Config exposing (ProjectInfo)
import Json.Decode as D

type Content
    = TranslationContent Translation
    | TranslationsContent (List Translation)
    | InterlinearsContent (List Interlinear)
    | DocContent { view : OneDoc, edit : Maybe CForm }
    | NewDocContent CForm
    | ProjectInfoContent ProjectInfo
    | ImportOptionsContent CForm
    | GlobalSettingsContent CForm


type alias Translation =
    { key : String
    , value : String
    , id : String
    }


translationsDecoder : D.Decoder (List Translation)
translationsDecoder =
    D.list translationDecoder


translationDecoder : D.Decoder Translation
translationDecoder =
    D.map3 Translation
        (D.field "key" D.string)
        (D.field "value" D.string)
        (D.field "id" D.string)
