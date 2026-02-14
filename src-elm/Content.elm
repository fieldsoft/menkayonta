module Content exposing (Content(..))

import Form.Global
import Form.Importer
import Form.Interlinear
import Form.Project
import Menkayonta exposing (Interlinear, OneDoc)


type Content
    = InterlinearsContent (List Interlinear)
    | ITV OneDoc
    | ITE Form.Interlinear.Model
    | GF Form.Global.Model
    | PR Form.Project.Model
    | IM Form.Importer.Model
