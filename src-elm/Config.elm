module Config exposing
    ( GlobalConfig
    , GlobalSettings
    , ProjectInfo
    , globalConfigDecoder
    , projectInfoDecoder
    )

import Json.Decode as D
import UUID


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
    , identifier : UUID.UUID
    , url : Maybe String
    , key : String
    }


globalConfigDecoder : D.Decoder GlobalConfig
globalConfigDecoder =
    D.map3 GlobalConfig
        (D.field "projects" <| D.list projectInfoDecoder)
        (D.field "name" <| D.nullable D.string)
        (D.field "email" <| D.nullable D.string)


projectInfoDecoder : D.Decoder ProjectInfo
projectInfoDecoder =
    D.map4 ProjectInfo
        (D.field "title" D.string)
        (D.field "identifier" UUID.jsonDecoder)
        (D.field "url" (D.nullable D.string))
        (D.field "key" D.string)
