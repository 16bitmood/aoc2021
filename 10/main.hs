import Data.List (sort)

(|>) :: (a -> b) -> (b -> c) -> a -> c
f |> g = g . f

opp :: Char -> Char
opp x = inv $ head [(a,b) | [a,b] <- ["()", "[]", "{}", "<>"], a == x || b == x]
    where inv (a,b) = if a == x then b else a

f :: [String] -> Int
f = map (foldl solve ([],0) |> snd) |> sum
    where
        solve (ys,x) e
            | e `elem` "([{<" =  (opp e : ys, x)
            | e == head ys = (tail ys, x)
            | otherwise = (tail ys, costOf e)
        costOf c = head [b | (a,b) <- [(')', 3), (']', 57), ('}', 1197), ('>', 25137)], a == c]

g :: [String] -> Int
g = map (foldl solve ([], False) |> completionCost)
    |> filter (/= 0) |> sort |> (\xs -> xs !! (length xs `div` 2))
    where
        solve (_, True) e = ([], True)
        solve (ys,x) e
            | e `elem` "([{<" =  (opp e : ys, x)
            | e == head ys = (tail ys, x)
            | otherwise = ([], True)
        completionCost (_, True) = 0
        completionCost (xs, False) = foldl (\a x -> 5*a + costOf x) 0 xs
        costOf c = head [b | (a,b) <- [(')', 1), (']', 2), ('}', 3), ('>', 4)], a == c]

main :: IO ()
main = interact $
    lines |> (\ls -> [f ls, g ls]) |> map show |> unlines