module Content exposing (Content(..))

import Menkayonta exposing (Interlinear, OneDoc)
import Config exposing (ProjectInfo)
import Json.Decode as D
import Form.Global
import Form.Interlinear
import Form.Project
import Form.Importer


type Content
    = InterlinearsContent (List Interlinear)
    | ITV OneDoc
    | ITE Form.Interlinear.Model
    | GF Form.Global.Model
    | PR Form.Project.Model
    | IM Form.Importer.Model
