import Data.List.Split (splitOn)
import Data.List (sortOn)

(|>) :: (a -> b) -> (b -> c) -> a -> c
f |> g = g . f

parseLine :: String -> Segment
parseLine line = case (splitOn "->" |> map (splitOn "," |> map read)) line of
    [[x1, y1], [x2, y2]] -> ((x1,y1),(x2,y2))
    _ -> error "Invalid Input"

type Point = (Int,Int)
type Segment = (Point, Point)

pointsIn :: Segment -> [Point]
pointsIn ((x1,y1), (x2,y2)) = [(x1 + k1*i, y1 + k2*i) | i <- [0..t]]
    where
        t = max (abs (x1-x2)) (abs (y1-y2))
        (k1, k2) = (signum (x2-x1), signum (y2-y1))

count :: [Point] -> [(Point, Int)]
count = sortOn fst |> s |> concat |> r
    where
        fstEq p q = fst p == fst q
        s (p:ps) = sortOn snd (takeWhile (fstEq p) (p:ps)): s (dropWhile (fstEq p) ps)
        s [] = []
        r (p:ps) = (p, 1+ length (takeWhile (== p) ps)):r (dropWhile (== p) ps)
        r [] =  []

f :: [Segment] -> [(Point, Int)]
f segments = count (concatMap pointsIn (filter cond segments))
    where
        cond ((x1,y1), (x2,y2)) = (x1 == x2) || (y1 == y2)

g :: [Segment] -> [(Point, Int)]
g segments =  count (concatMap pointsIn segments)

main :: IO ()
main = interact $
    lines |> map parseLine
        |> (\xs ->  [f  xs, g  xs])
        |> map (map snd |> filter (>= 2) |> length |> show)
        |> unlines