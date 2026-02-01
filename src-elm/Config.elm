module Config exposing (GlobalConfig, GlobalSettings, ProjectInfo)


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
