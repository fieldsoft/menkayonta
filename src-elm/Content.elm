module Content exposing (Content(..))

import Form.Global
import Form.Importer
import Form.Interlinear
import Form.Project
import Form.Sequence
import Display.Note
import Menkayonta
    exposing
        ( Composite
        , Interlinear
        , Person
        , Sequence
        )


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
file import form; and 'PL', a person object.

-}
type Content
    = ITS (List Interlinear)
    | SQS (List Sequence)
    | ITV Composite
    | SQV Composite
    | ITE Form.Interlinear.Model
    | SQE Form.Sequence.Model
    | PLS (List Person)
    | NTV Display.Note.Model
    | GF Form.Global.Model
    | PR Form.Project.Model
    | IM Form.Importer.Model
