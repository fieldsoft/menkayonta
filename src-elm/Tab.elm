module Tab exposing
    ( Direction(..)
    , Msg(..)
    , Path
    , Ventana
    , VentanaParams
    , Ventanas
    , VisVentanas
    , Vista
    , Vistas
    )

import Content exposing (Content)
import Dict exposing (Dict)


type Msg
    = Clone
    | Close
    | Focus Path
    | Goto Path
    | Move Direction
    | New Ventana


{-| A Ventana supplies the title and a referrence to a Vista, which is
an identifier for some viewable content. I use Spanish when there are
already commonly referred to object or concepts such as "window" or
"view".
-}
type alias Ventana =
    { title : String
    , fullTitle : String
    , vista : String
    , params : VentanaParams
    }


type alias VentanaParams =
    { length : Int
    , searchString : String
    , edit : Bool
    }


{-| All of the viewable content associated with a tab.
-}
type alias Ventanas =
    Dict Path Ventana


{-| Viewable content.
-}
type alias Vista =
    { project : String
    , kind : String
    , identifier : String
    , content : Content
    }


type alias Vistas =
    Dict String Vista


{-| The path to a tab, used for operations on tabs.
-}
type alias Path =
    ( Int, ( Int, Int ) )


{-| Currently visible Ventanas.
-}
type alias VisVentanas =
    Dict ( Int, Int ) Int


type Direction
    = Left
    | Right
    | Up
    | Down
