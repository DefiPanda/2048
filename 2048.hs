import Data.List
import System.Random
import Control.Monad(when) 

-- slide the grids in a list in left direction
shift_left_row :: [Int] -> [Int]
shift_left_row [] = []
shift_left_row [x] = [x]
shift_left_row (x:y:xs) | x == 0 = shift_left_row (y:xs) ++ [0]
                        | y == 0 = shift_left_row (x:xs) ++ [0]
                        | x /= y = [x] ++ shift_left_row (y:xs)
                        | otherwise = [2 * x] ++ shift_left_row xs ++ [0]

-- slide the grids in the map in left direction
slide_left :: [[Int]] -> [[Int]]
slide_left rows = [shift_left_row row | row <- rows]

-- rotate the map clockwise
rotate_right :: [[Int]] -> [[Int]]
rotate_right = map reverse . transpose

-- rotate the map counter-clockwise
rotate_left :: [[Int]] -> [[Int]]
rotate_left = reverse . transpose

-- slide the grids in the map in a certain direction
slide :: [[Int]] -> [Char] -> [[Int]]
slide rows direction | direction == "left" = slide_left rows
                     | direction == "right" = rotate_right (rotate_right (slide_left (rotate_right (rotate_right rows))))
                     | direction == "up" = rotate_right (slide_left (rotate_left rows))
                     | direction == "down" = rotate_left (slide_left (rotate_right rows))
                     | otherwise = rows

-- check if the map contains any zero
has_zero :: [[Int]] -> Bool
has_zero rows = foldl (\acc row -> acc || elem 0 row) False rows

-- check if the game is over
game_over :: [[Int]] -> Bool
game_over rows | has_zero rows == True = False
               | slide rows "left" /= rows = False
               | slide rows "right" /= rows = False
               | slide rows "up" /= rows = False
               | slide rows "down" /= rows = False
               | otherwise = True

-- count number of 0s in a list
count_zeroes_in_row :: [Int] -> Int
count_zeroes_in_row row = foldl (\acc grid -> if grid == 0 then acc + 1 else acc) 0 row

-- count number of 0s in the map
count_zeroes_in_rows :: [[Int]] -> Int
count_zeroes_in_rows rows = foldl (\acc row -> acc + count_zeroes_in_row row) 0 rows

-- return 4 if x is 1
two_or_four :: Int -> Int
two_or_four x = if x == 1 then 4 else 2

-- flatten nested list to list
flatten_rows :: [[Int]] -> [Int]
flatten_rows rows = foldl (\acc row -> acc ++ row) [] rows

-- find the index of nth zero in a flattened map
find_nth_zero :: [[Int]] -> Int -> Int
find_nth_zero rows grid = [i | i <- [0..length flat - 1], flat!!i == 0 ] !! (grid - 1)
                                 where flat = flatten_rows rows

-- add a tile to a empty cell to map
add_tile :: [[Int]] -> Int -> Int -> [[Int]]
add_tile rows num grid | count_zeroes_in_rows rows < grid = rows
                       | otherwise = [take 4 changed, take 4 (drop 4 changed), take 4 (drop 8 changed), drop 12 changed]
                                   where flat = flatten_rows rows
                                         cell = find_nth_zero rows grid
                                         changed = take cell flat ++ [num] ++ drop (cell + 1) flat                                 

-- check whether input direction is correct
valid_input :: [Char] -> Bool
valid_input direction = direction == "left" || direction == "right" || direction == "up" || direction == "down"

-- REPL
loop :: [[Int]] -> IO ()
loop rows = when (game_over rows == False)
                              (do direction <- getLine
                                  gen <- getStdGen
                                  setStdGen (snd (next gen))
                                  let num= fst (randomR (1, 10) gen :: (Int, StdGen))
                                  let grid = if (count_zeroes_in_rows rows > 0) then fst (randomR (1, count_zeroes_in_rows rows) gen :: (Int, StdGen)) else -1
                                  let slided_rows = if (valid_input direction) then slide rows direction else rows
                                  let added_tile_rows = if grid /= -1 && (valid_input direction) && (slided_rows /= rows) then (add_tile slided_rows (two_or_four num) grid) else slided_rows
                                  if (valid_input direction == False) then print "Please enter valid direction" else putStr ""
                                  print added_tile_rows
                                  loop added_tile_rows)

-- init game and call REPL
main = do
    let rows = [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]
    gen <- getStdGen
    setStdGen (snd (next gen))
    let randNumber = fst (randomR (1, 10) gen :: (Int, StdGen))
    let randGrid = fst (randomR (1, count_zeroes_in_rows rows) gen :: (Int, StdGen))
    let added_one_tile = add_tile rows (two_or_four randNumber) randGrid
    gen <- getStdGen
    let randNumber = fst (randomR (1, 10) gen :: (Int, StdGen))
    let randGrid = fst (randomR (1, count_zeroes_in_rows added_one_tile) gen :: (Int, StdGen))
    let added_two_tiles = add_tile added_one_tile (two_or_four randNumber) randGrid
    print added_two_tiles
    loop added_two_tiles 
    print "Game Over"