import Data.List (sort)
import Data.List.Split (splitOn)
import Data.IntMap (IntMap, fromList, (!), insert)

(|>) :: (a -> b) -> (b -> c) -> a -> c
f |> g = g . f

f :: [Int] -> Int
f xs =  sum $ map ((median -) |> abs) xs
    where 
        median = xs !! (length xs `div` 2)

g :: [Int] -> Int
g xs = minimum ys
    where
        ys = map (dSum |> sum) xs
        dSum x = map ((x -) |> abs |> sumN) xs
        sumN n = (n*(n+1)) `div` 2

main :: IO ()
main = interact $
    splitOn "," |> map read |> sort |> (\xs -> [f xs, g xs]) |> map show |> unlines