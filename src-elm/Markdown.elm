module Markdown exposing (render)

import Html
import Markdown.Parser
import Markdown.Renderer as Renderer
import Parser.Advanced
import Parser


render : String -> Html.Html msg
render raw =
    let
        renderResult : Result String (List (Html.Html msg))
        renderResult =
            raw
                |> Markdown.Parser.parse
                |> Result.mapError deadEndsToString
                |> Result.andThen
                   (\ast ->
                        Renderer.render Renderer.defaultHtmlRenderer ast
                   )

    in
    case renderResult of
        Ok r ->
            Html.div [] r

        Err e ->
            Html.text e
    

deadEndsToString : (List (Parser.Advanced.DeadEnd String Parser.Problem)) -> String
deadEndsToString deadEnds =
    deadEnds
        |> List.map Markdown.Parser.deadEndToString
        |> String.join "\n"
