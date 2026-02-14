module Config exposing
    ( GlobalConfig
    , ProjectInfo
    , globalConfigDecoder
    )

import Json.Decode as D
import UUID


type alias GlobalConfig =
    { projects : List ProjectInfo
    , name : String
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
        (D.field "name" D.string)
        (D.field "email" D.string)


projectInfoDecoder : D.Decoder ProjectInfo
projectInfoDecoder =
    D.map4 ProjectInfo
        (D.field "title" D.string)
        (D.field "identifier" UUID.jsonDecoder)
        (D.field "url" (D.nullable D.string))
        (D.field "key" D.string)
