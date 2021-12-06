import Data.List.Split (splitOn)
import Data.IntMap (IntMap, fromList, (!), insert)

(|>) :: (a -> b) -> (b -> c) -> a -> c
f |> g = g . f

iterDay :: IntMap Integer -> IntMap Integer
iterDay m = fromList $ [(i-1, m!i) | i <- [1..6]] ++ 
    [(6, m!7 + m!0)] ++ [(7,m!8)] ++ [(8, m!0)]

countFishes :: IntMap Integer -> [Int] -> IntMap Integer
countFishes = foldl (\m x -> insert x (m ! x + 1) m)

main :: IO ()
main = interact $
    splitOn "," |> map read
    |> countFishes initMap |> (\xs -> [iterate iterDay xs !! 80, iterate iterDay xs !! 256])
    |> map (sum |> show) |> unlines
    where
        initMap = fromList [(i,0) | i <- [0..8]]