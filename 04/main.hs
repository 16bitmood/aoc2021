{-# LANGUAGE TupleSections #-}
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe, isNothing)

(|>) :: (a -> b) -> (b -> c) -> a -> c
f |> g = g . f

type Board = [(Bool, Int)]

rows :: Board -> [[(Bool, Int)]]
rows [] = []
rows xs = take 5 xs : rows (drop 5 xs)

cols :: Board -> [[(Bool, Int)]]
cols [] = []
cols xs = [map (xs !!) [i, i + 5 .. 24] | i <- [0 .. 4]]

parseBoard :: String -> Board
parseBoard = splitOn "\n" |> map (words |> map read)|> concat |> map (False, )

parse :: String -> ([Int], [Board])
parse xs = case splitOn "\n\n" xs of
        (nums:boards) -> (map read (splitOn "," nums), map parseBoard boards)
        _ -> error "Invalid Input"

mark :: Int -> Board -> Board
mark n =  map (\(b, x) -> if x == n then (True, x) else (b,x))

sumRest :: Board -> Int
sumRest = foldl (\acc (b, x) -> if b then acc else acc + x) 0

haveWon :: Board -> Maybe Int
haveWon board = if any and rowsAndCols
    then Just (sumRest board)
    else Nothing
    where
        rowsAndCols = map (map fst) (rows board ++ cols board)

f :: [Int] -> [Board] -> Int
f (n:ns) boards = case mapMaybe haveWon marked of
    s:_ -> n*s
    []  -> f ns marked
    where
        marked = map (mark n) boards

f [] _ = error "Invalid Input"


solveSingle :: [Int] -> Board -> Int
solveSingle (n:ns) board = case haveWon marked of
    Just s -> n*s
    Nothing -> solveSingle ns marked
    where marked = mark n board

solveSingle [] _ = error "Invalid Input"

g :: [Int] -> [Board] -> Int
g ns [board] = solveSingle ns board

g (n:ns) boards = g ns (filter (haveWon |> isNothing) marked)
    where
        marked = map (mark n) boards

g [] _ = error "Invalid Input"


main :: IO ()
main = interact $
    parse |> (\(nums, boards) ->  [f nums boards, g nums boards]) |> map show |> unlines