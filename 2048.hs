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

-- count number of 0s in a list
count_zeroes_in_row :: [Int] -> Int
count_zeroes_in_row row = foldl (\acc grid -> if grid == 0 then acc + 1 else acc) 0 row

-- count number of 0s in the map
count_zeroes_in_rows :: [[Int]] -> Int
count_zeroes_in_rows rows = foldl (\acc row -> acc + count_zeroes_in_row row) 0 rows

-- check if the game is over
game_over :: [[Int]] -> Bool
game_over rows = slide rows "left" == rows && slide rows "right" == rows && slide rows "up" == rows && slide rows "down" == rows

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

-- generate a random number in range [1, range]
randomNum :: Int -> StdGen -> Int
randomNum range gen = fst (randomR (1, range) gen :: (Int, StdGen))

-- generate insert a random number (with 10% chance of being 4 and 90% chance of being 2), and insert to a random empty cell
new_map :: [[Int]] -> StdGen -> [[Int]]
new_map rows gen = add_tile rows (if dice == 1 then 4 else 2) (randomNum (count_zeroes_in_rows rows) gen) where dice = randomNum 10 gen

-- check whether input direction is correct
valid_input :: [Char] -> Bool
valid_input direction = direction == "left" || direction == "right" || direction == "up" || direction == "down"

-- REPL
loop :: [[Int]] -> IO ()
loop rows = when (game_over rows == False)
                              (do putStrLn $ concat [(show row) ++ "\n"| row <- rows]
                                  direction <- getLine
                                  gen <- getStdGen
                                  setStdGen (snd (next gen))
                                  let slided_rows = if (valid_input direction) then slide rows direction else rows
                                  let added_tile_rows = if count_zeroes_in_rows rows > 0 && (valid_input direction) && (slided_rows /= rows) then new_map slided_rows gen else slided_rows
                                  if (valid_input direction == False) then putStrLn "Please enter valid direction" else putStrLn ""
                                  loop added_tile_rows)

-- init game and call REPL
main :: IO ()
main = do
    let rows = [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]
    gen <- getStdGen
    let added_one_tile = new_map rows gen
    setStdGen (snd (next gen))
    gen <- getStdGen
    loop (new_map added_one_tile gen)
    putStrLn "Game Over"