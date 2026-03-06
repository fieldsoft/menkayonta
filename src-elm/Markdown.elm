module Markdown exposing (render)

import Html
import Markdown.Parser as Markdown
import Markdown.Renderer as Renderer


render : String -> Html.Html msg
render raw =
    let
        renderResult : Result String (List (Html.Html msg))
        renderResult =
            raw
                |> Markdown.parse
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
    

deadEndsToString deadEnds =
    deadEnds
        |> List.map Markdown.deadEndToString
        |> String.join "\n"
