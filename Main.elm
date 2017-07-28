import Time exposing (Time, second)
import Html exposing (Html, button, div, h1, h3, ul, li)
import Html.Attributes as Attr
import Color exposing (Color, rgb)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Random as R
import Json.Decode as Json
import Keyboard
import Tetris
import Graphics
import Blocks exposing (blocks, blockT)

type Msg = Tick | Left | Right | Up | Down | Nothing | Drop | NewBlock Int
type alias State = {
    game: Tetris.Tetris Color,
    block: Tetris.Tetris Color,
    bi: Int, bj: Int,
    score: Int,
    gameOver: Int}

type alias Step = (State, Cmd Msg)

main = Html.program {
    init=init,
    view=view,
    update=update,
    subscriptions=subscriptions}

init : Step
init = ({game=Tetris.init, score=0, block=blockT, bi=0, bj=0, gameOver=-1}, randomBlock)

randomBlock : Cmd Msg
randomBlock = R.generate NewBlock (R.int 0 (List.length blocks - 1))

doTick : State -> Step
doTick state =
    if state.gameOver > 0
    then return {state | gameOver=state.gameOver - 1}
    else let i = state.bi + 1
         in if Tetris.collide i state.bj state.block state.game
            then let game = Tetris.blit state.bi state.bj state.block state.game
                 in ({state | game=game}, randomBlock)
            else return {state | bi=i}

doDrop : State -> Step
doDrop state =
    let (r, c) = doTick state
    in if c == Cmd.none
       then doDrop r
       else (r, c)

move : Int -> State -> Step
move direction state =
    let j = state.bj + direction
        collision = Tetris.collide state.bi j state.block state.game
        onGrid = j + state.block.cols <= state.game.cols && j >= 0
        res = if onGrid && not collision
              then {state | bj=j}
              else state
    in return res

rotate : State -> Step
rotate state = return {state | block=Tetris.rotate state.block}

newBlock : Int -> State -> Step
newBlock idx state =
    let blk = Maybe.withDefault blockT (List.drop idx blocks |> List.head)
        (lines, game) = Tetris.cleanLines state.game
        points = lines*lines
        score = state.score + points
        (bi, bj) = (-1, state.game.cols // 2)
    in return {game=game, block=blk, bi=bi, bj=bj, gameOver=-1, score=score}

return : State -> Step
return state = if state.gameOver == 0 then
                   init
               else if Tetris.gameOver state.game && state.gameOver < 0  then
                   ({state | gameOver=5}, Cmd.none)
               else
                   (state, Cmd.none)

update : Msg -> State -> Step
update msg = 
    case msg of
        Tick -> doTick
        Down -> doTick
        Drop -> doDrop
        Left -> move -1
        Right -> move 1
        Up -> rotate
        NewBlock i -> newBlock i
        _ -> return

mapKeyboard : Int -> Msg
mapKeyboard x = 
    case x of
        13 -> Drop
        37 -> Left
        39 -> Right
        38 -> Up
        40 -> Down
        _ -> Nothing

subscriptions : State -> Sub Msg
subscriptions state = Sub.batch [ Time.every second (\_ -> Tick)
                                , Keyboard.presses mapKeyboard]

width = 200
height = 400
toSvg state =
    let display = Tetris.blit state.bi state.bj state.block state.game
    in Graphics.toSvg width height display

viewGame state = [
    h1 [] [ text "Tetrelm" ],
    h3 [] [ text <| (toString state.score)  ++ " points"],
    toSvg state,
    ul [] [
        li [] [text "LEFT / RIGHT: move piece left or right"],
        li [] [text "UP: move piece down"],
        li [] [text "DOWN: move piece down"],
        li [] [text "ENTER: drop piece"]]]

viewFail = [h3 [Attr.style [("color", "red"), ("font-size", "200%")]] [text "GAME OVER"]]

view : State -> Html Msg
view state = 
    let content = if Tetris.gameOver state.game
                  then viewFail
                  else viewGame state
    in div [Attr.style [("margin", "20px")]] content
