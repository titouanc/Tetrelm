module Tetris exposing (Tetris, map, flatNonVoid,
                                put, blit, collide, cleanLines, gameOver,
                                rotate, tetris, init)

import Better exposing (..)
import Color

-- A tetris is basically a fixed size matrix with a void element
type alias Tetris a = {
    cols: Int,
    rows: Int, 
    grid: List (List a),
    void: a
}

-- Create a new empty tetris
tetris : Int -> Int -> a -> Tetris a
tetris cols rows void = {
    cols=cols,
    rows=rows,
    void=void,
    grid=List.repeat rows <| List.repeat cols <| void}

-- Create a standard tetris
init : Tetris Color.Color
init = tetris 10 20 Color.black

-- Map over the tetris values
map : (a -> b) -> Tetris a -> Tetris b
map f t = {t | grid=List.map (List.map f) t.grid, void=f t.void}

-- Map over the tetris values with indexes
imap : b -> (Int -> Int -> a -> b) -> Tetris a -> Tetris b
imap void f t =
    let grid = List.indexedMap (\i row -> List.indexedMap (\j x -> f i j x) row) t.grid
    in {t | grid=grid, void=void}

-- Like imap, but return a flat list of all non void items insted of a new tetris
flatNonVoid : (Int -> Int -> a -> b) -> Tetris a -> List b
flatNonVoid f t =
    let g i j x = if x == t.void then Nothing else f i j x |> Just
    in imap Nothing g t |> .grid |> List.concat |> filterMaybe

-- The type is explicit enough
filterMaybe : List (Maybe a) -> List a
filterMaybe lst =
    case lst of
        [] -> []
        x::xs -> case x of
            Nothing -> filterMaybe xs
            Just c -> c::filterMaybe xs

-- Split the list at given index, return 2 lists
split : Int -> List a -> (List a, List a)
split i lst = (List.take i lst, List.drop i lst)

-- Replace a value in a 2-dimensional list
replace2d : List (List a) -> Int -> Int -> a -> List (List a)
replace2d lst i j val =
    let (top, rest) = split i lst
    in case rest of
        row::bottom ->
            let (left, rest_) = split j row
                (_, right) = split 1 rest_
            in top ++ [left ++ [val] ++ right] ++ bottom
        [] -> top

-- Put value at given indexand return updated tetris
put : Int -> Int -> a -> Tetris a -> Tetris a
put row col item t = 
    if row < 0 || col < 0 || row >= t.rows || col >= t.cols
    then t
    else {t | grid=replace2d t.grid row col item}

-- Copies the content of first tetris in second and return updated second
blit : Int -> Int -> Tetris a -> Tetris a -> Tetris a
blit i j src dst =
    let translate y x item = (i+y, j+x, item)
        positions = flatNonVoid translate src
    in List.foldl (\(i,j,c) r -> put i j c r) dst positions

unvoid : Tetris a -> Tetris Bool
unvoid t = map (\x -> x /= t.void) t

collide : Int -> Int -> Tetris a -> Tetris a -> Bool
collide i j src dst =
    if (i + src.rows) > dst.rows
    then True
    else let s = unvoid src |> .grid
             d = unvoid dst |> .grid |> List.drop i
             cmpRow src dst = List.foldl (||) False <| List.map2 (&&) src (List.drop j dst)
         in List.foldl (||) False <| List.map2 cmpRow s d


maybeList : List (Maybe a) -> Maybe (List a)
maybeList lst =
    case lst of
        [] -> Just []
        mx::xs -> case mx of
            Nothing -> Nothing
            Just x -> maybeList xs ?> ((::) x)

transpose : List (List a) -> List (List a)
transpose lst =
    case List.map List.head lst |> maybeList of
        Nothing -> []
        Just x -> x :: transpose (List.map (List.drop 1) lst)


rotate : Tetris a -> Tetris a
rotate t =
    let grid = List.reverse t.grid |> transpose
    in {t | grid=grid, cols=t.rows, rows=t.cols}


cleanLines : Tetris a -> (Int, Tetris a)
cleanLines t =
    let r = List.reverse t.grid
        occupied = (/=) t.void
        keep line = List.all occupied line |> not
        clean = List.filter keep r
        n = List.length r - List.length clean
        pad = List.repeat n (List.repeat t.cols t.void)
    in (n, {t | grid=pad ++ List.reverse clean})


gameOver : Tetris a -> Bool
gameOver t =
    let top = List.head t.grid |> Maybe.withDefault []
    in List.any ((/=) t.void) top
