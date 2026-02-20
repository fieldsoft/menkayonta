module Content exposing (Content(..))

import Form.Global
import Form.Importer
import Form.Interlinear
import Form.Project
import Menkayonta exposing (Interlinear, Composite)


{-| The `Content` type describes all of the viewable information
associated with a `Tab.Vista`. In order to avoide excessively long
names, an abbreviation is used for the type constructor, which has a
convention. Something that has as the third letter 'S' is a
listing. Something with 'V' is a non-form display. The code for the
display of these is either in `Main` or under `Display`. An 'E'
indicates that the content is a form. No third letter indicates that
there is no ambiguity.

The abbreviations are as follows: 'IT', interlinear gloss; 'GF',
global confituration; 'PR', project specific configuration; 'IM', the
file import form.
-}
type Content
    = ITS (List Interlinear)
    | ITV Composite
    | ITE Form.Interlinear.Model
    | GF Form.Global.Model
    | PR Form.Project.Model
    | IM Form.Importer.Model
