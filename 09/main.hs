{-# LANGUAGE TupleSections #-}
import Data.List (sort, permutations, elemIndex, sortOn, groupBy, group)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe, isJust)
import Data.Char (digitToInt)
import Data.Array (Array, array, (!), listArray, bounds, (//), elems)

(|>) :: (a -> b) -> (b -> c) -> a -> c
f |> g = g . f

type Matrix = Array Int (Array Int (Int, Int))

parse :: String -> Matrix
parse txt = splitOn "\n"
            |> map (map digitToInt |> map (, -1) |> listArray (1,w))
            |> listArray (1,h) $ txt
    where
        w = length $ takeWhile (/= '\n') txt
        h = 1 + length (filter (== '\n') txt)

dimensions :: Matrix -> (Int, Int)
dimensions m = (snd (bounds m), snd (bounds (m!1)))

indexes :: Matrix -> [(Int,Int)]
indexes m = [(i,j) | i <- [1..h], j <- [1..w]]
    where (h, w) = dimensions m

possibleMoves :: Matrix -> (Int,Int) -> [(Int, Int)]
possibleMoves m p = filter isPossible (jumpFrom p)
    where
        moves = [(1,0),(0,1),(-1,0),(0,-1)]
        jumpFrom (i,j) = map (\(a,b) -> (a + i, b + j)) moves
        isPossible (i,j) = (1 <= i && i <= h) && (1 <= j && j <= w)
        (h,w) = dimensions m

isLowest :: Matrix -> (Int,Int) -> Bool
isLowest mat (i,j) = and [fst (mat!i!j) < fst (mat!p!q) | (p,q) <- possibleMoves mat (i,j)]

unmarked :: Matrix -> (Int,Int) -> Bool
unmarked m (i,j) = snd (m!i!j) == -1

groupCounts :: Matrix -> [Int]
groupCounts = elems  |> concatMap elems 
            |> map snd |> filter (/= -1) |> sort |> group |> map length

bfs :: Matrix -> [(Int, Int)] -> Int -> Matrix
bfs m [] _ = m
bfs m ((i,j):ps) x = bfs (mark (i,j) x) ps' x
    where
        mark (i,j) x = m // [(i, (m!i) // [(j, (fst (m!i!j), x))])]
        ps' = [(p,q) | (p,q) <- possibleMoves m (i,j), fst (m!p!q) /= 9, snd (m!p!q) == -1] ++ ps

iterBFS :: Matrix -> Int -> Matrix
iterBFS m x = case filter (\p -> unmarked m p && isLowest m p) (indexes m) of
    [] -> m
    (p:ps) -> iterBFS (bfs m [p] x) (x+1)

f :: Matrix -> Int
f m = sum [1 + fst (m!i!j)| (i,j) <- filter (isLowest m) (indexes m)]

g :: Matrix -> Int
g m = case (sort |> reverse) $ groupCounts (iterBFS m 0) of
        a:b:c:_ -> a*b*c
        _ -> error "Not Enough Sinks"

main :: IO ()
main = interact $
    parse  |> (\m -> [f m, g m]) |> map show |> unlines