module Blocks exposing (blocks, blockT, blockL, blockJ, block4, blockZ, blockI, blockO)

import Color
import Tetris exposing (Tetris, tetris, map, put)

toColor : Color.Color -> Color.Color -> Int -> Color.Color
toColor zero one x = if x == 0 then zero else one

colorize : Color.Color -> Tetris Int -> Tetris Color.Color
colorize c = map (toColor (Color.rgb 127 127 127) c)

blockT = tetris 3 2 0 |> put 0 0 1 |> put 0 1 1 |> put 0 2 1 |> put 1 1 1
                      |> colorize Color.yellow
blockL = tetris 3 2 0 |> put 0 0 1 |> put 0 1 1 |> put 0 2 1 |> put 1 0 1
                      |> colorize Color.red
blockJ = tetris 3 2 0 |> put 0 0 1 |> put 0 1 1 |> put 0 2 1 |> put 1 2 1
                      |> colorize (Color.rgb 250 0 250)
block4 = tetris 2 3 0 |> put 0 0 1 |> put 1 0 1 |> put 1 1 1 |> put 2 1 1
                      |> colorize Color.blue
blockZ = tetris 2 3 0 |> put 0 1 1 |> put 1 1 1 |> put 1 0 1 |> put 2 0 1
                      |> colorize (Color.rgb 0 250 250)
blockI = tetris 1 4 0 |> put 0 0 1 |> put 1 0 1 |> put 2 0 1 |> put 3 0 1
                      |> colorize Color.green
blockO = tetris 2 2 0 |> put 0 0 1 |> put 1 0 1 |> put 0 1 1 |> put 1 1 1
                      |> colorize Color.white

blocks = [blockT, blockL, blockJ, block4, blockZ, blockI, blockO]
