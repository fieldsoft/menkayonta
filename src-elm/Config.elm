module Config exposing
    ( GlobalConfig
    , GlobalSettings
    , ProjectInfo
    , globalConfigDecoder
    , projectInfoDecoder
    )

import Json.Decode as D


type alias GlobalConfig =
    { projects : List ProjectInfo
    , name : Maybe String
    , email : Maybe String
    }


{-| This is the subset of global configuration that is not specific to
a project.
-}
type alias GlobalSettings =
    { name : String
    , email : String
    }


type alias ProjectInfo =
    { title : String
    , identifier : String
    , url : Maybe String
    }


globalConfigDecoder : D.Decoder GlobalConfig
globalConfigDecoder =
    D.map3 GlobalConfig
        (D.field "projects" <| D.list projectInfoDecoder)
        (D.field "name" <| D.nullable D.string)
        (D.field "email" <| D.nullable D.string)


projectInfoDecoder : D.Decoder ProjectInfo
projectInfoDecoder =
    D.map3 ProjectInfo
        (D.field "title" D.string)
        (D.field "identifier" D.string)
        (D.field "url" (D.nullable D.string))
