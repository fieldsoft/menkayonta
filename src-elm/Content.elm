module Content exposing (Content(..))

import Menkayonta exposing (Interlinear, OneDoc)
import Form exposing (CForm)
import Config exposing (ProjectInfo)
import Json.Decode as D
import Form.Global
import Form.Interlinear
import Form.Project


type Content
    = InterlinearsContent (List Interlinear)
    | ITV OneDoc
    | ITE Form.Interlinear.Model
    | ImportOptionsContent CForm
    | GF Form.Global.Model
    | PR Form.Project.Model
