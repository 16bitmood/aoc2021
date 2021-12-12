import Data.List (sortOn, groupBy)
import Data.Char (isLower)
import Data.List.Split (splitOn)
import Data.Map (Map, keys, fromList, lookup)
import Data.Set (Set, union, insert, empty, size)
import qualified Data.Map as M
import qualified Data.Set as S

(|>) :: (a -> b) -> (b -> c) -> a -> c
f |> g = g . f

type Graph = Map String [String]

parse :: String -> Graph
parse = lines |> map (splitOn "-" |>  makeEdges)
        |> concat |> sortOn fst |> groupBy (\p q -> fst p == fst q) |> map clean |> fromList
    where
        makeEdges [x,y] = [(x,y), (y,x)]
        makeEdges _ = error "Invalid Input"
        clean xs = (fst (head xs), map snd xs)

count :: Eq a => a -> [a] -> Int
count x = filter (== x) |> length

allPaths :: Graph -> Map String Int -> Set String
allPaths g permitted = iter "start" ["start"] empty
    where
        iter node revPath@("end":_) allps = insert (unwords revPath) allps
        iter node prevPath allps = foldl union empty nextIters
            where
                nextIters = case M.lookup node g of
                    Just ns -> [iter n (n:prevPath) allps | n <- ns, isPermitted (1 + count n prevPath) (M.lookup n permitted)]
                    _ -> error "Invalid Graph"
        isPermitted c (Just p) = p == -1 || c <= p
        isPermitted _ Nothing = False

main :: IO ()
main = interact $
    parse |> (\graph -> [f graph, g graph]) |> map show |> unlines
    where
        defaultMap graph = fromList [(e, if all isLower e then 1 else -1) | e <- keys graph]

        f graph = size $ allPaths graph (defaultMap graph)
        g graph = size $ foldl union empty [allPaths graph (M.insert n 2 (defaultMap graph)) | n <- keys graph, all isLower n, n /= "start"]
