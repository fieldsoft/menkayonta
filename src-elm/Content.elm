module Content exposing (Content(..), translationsDecoder, Translation)

import Menkayonta exposing (Interlinear, OneDoc)
import Form exposing (CForm)
import Config exposing (ProjectInfo)
import Json.Decode as D
import Form.Global
import Form.Interlinear

type Content
    = InterlinearsContent (List Interlinear)
    | ITV OneDoc
    | ITE Form.Interlinear.Model
    | ProjectInfoContent CForm
    | ImportOptionsContent CForm
    | GF Form.Global.Model


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
