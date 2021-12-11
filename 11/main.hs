import Data.Array (Array, array, listArray, (!), (//), elems, bounds)
import Data.Char (digitToInt)
import Data.List (sort)
import Data.Set (Set, empty, insert, member, difference, union, fromList)
import qualified Data.Set as S (null, elems)

(|>) :: (a -> b) -> (b -> c) -> a -> c
f |> g = g . f

type Matrix = Array Int (Array Int Int)

parse :: String -> Matrix
parse txt = lines
            |> map (map digitToInt |> listArray (1,w))
            |> listArray (1,h) $ txt
    where
        w = length $ takeWhile (/= '\n') txt
        h = 1 + length (filter (== '\n') txt)

getM :: Matrix -> (Int, Int) -> Int
getM m (i,j) = m!i!j

setM :: Matrix -> (Int,Int) -> Int -> Matrix
setM m (i,j) x = m // [(i,(m!i) // [(j,x)])]

sizeM :: Matrix -> (Int, Int)
sizeM mat = (snd (bounds mat), snd (bounds (mat!1)))

sizeM' :: Matrix -> Int
sizeM' mat = uncurry (*) (sizeM mat)

adjacentM :: Matrix -> (Int,Int) -> [(Int, Int)]
adjacentM m (i,j) =  filter (\p -> inBounds p &&  (p /= (i,j))) [(i+x,j+y) | x <- [0,1,-1], y <- [0,1,-1]]
    where
        (h,w) = sizeM m
        inBounds (a,b) = (1 <= a && a <= h) && (1 <= b && b <= w)

indicesM :: Matrix -> [(Int, Int)]
indicesM m = [(i,j) | i <- [1..h], j <- [1..w]]
    where (h,w) = sizeM m

flashAll :: Matrix -> (Matrix, Set (Int, Int))
flashAll m = flashIter m (fromList (aboutToFlash m)) empty
    where
    flashIter m currFlash prevFlash
        | S.null currFlash = (m, prevFlash)
        | otherwise = flashIter nextM toFlashNext (prevFlash `union` currFlash)
        where
            nextM = foldl succAdjacent m (S.elems currFlash)
            toFlashNext = fromList (aboutToFlash nextM) `difference` (prevFlash `union` currFlash)
    succAdjacent m p = foldl (\m p -> setM m p (succ (getM m p))) m (adjacentM m p)
    aboutToFlash m = (indicesM |> filter (\p -> getM m p > 9)) m

step :: Matrix -> (Matrix, Int)
step matrix = (incrementAll |> flashAll |> zeroFlashed) matrix
    where
        (h,w) = sizeM matrix
        incrementAll = elems |> map (elems |> map succ |> listArray (1,w)) |> listArray (1,h)
        zeroFlashed (m, flashed) = foldl (\(m,x) p -> (setM m p 0, x+1)) (m, 0) flashed

main :: IO ()
main = interact $
    parse |> (\m -> [f m, g m]) |> map show  |> unlines
    where
        steps m = (m,0) : map (fst |> step) (steps m)
        f = steps |> tail |> take 100 |> map snd |> sum
        g = steps |> takeWhile (\(m, x) -> x /= sizeM' m) |> length