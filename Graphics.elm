module Graphics exposing (toSvg, toCss)

import Html exposing (Html)
import Color
import Svg exposing (svg, rect)
import Svg.Attributes as Attr
import Tetris exposing (Tetris, flatNonVoid, map)

toCss : Color.Color -> String
toCss c =
    let {red, green, blue} = Color.toRgb c
        colors = [red, green, blue]
    in "rgb(" ++ (String.join "," <| List.map toString colors) ++ ")"

-- Render Tetris as Svg
toSvg : Float -> Float -> Tetris Color.Color -> Html a
toSvg width height tetris =
    let dh = height / toFloat tetris.rows
        dw = width / toFloat tetris.cols
        css = map toCss tetris
        drawRect i j c =
            let top = toFloat i * dh |> toString
                left = toFloat j * dw |> toString
            in rect [ Attr.x left
                    , Attr.y top
                    , Attr.width <| toString dw
                    , Attr.height <| toString dh
                    , Attr.class "animated"
                    , Attr.fill c] []
        back = rect [Attr.x "0"
                    , Attr.y "0"
                    , Attr.width <| toString width
                    , Attr.height <| toString height
                    , Attr.fill css.void] []
        shapes = [back] ++ flatNonVoid drawRect css
    in svg [Attr.width <| toString width, Attr.height <| toString height] shapes
