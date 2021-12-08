import Data.List (sort, permutations, elemIndex)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe, isJust)

(|>) :: (a -> b) -> (b -> c) -> a -> c
f |> g = g . f

parseLine :: String -> ([String], [String])
parseLine line = case splitOn "|" |> map (splitOn " " |> filter (/= "")) $ line of
    [xs,ys] -> (xs,ys)
    _ -> error "Invalid Input"

-- Part 1
f :: [([String], [String])] -> Int
f = map solve |> sum
    where
        solve (_, ys) = uniqCount ys
        uniqCount = filter (\y -> length y `elem` [2,4,3,7]) |> length

-- Part 2
getDigit :: String -> Maybe Int
getDigit str = elemIndex (sort str) nums
    where
    nums = ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

getInt :: [String] -> Maybe Int
getInt xs = if length ds == length xs then Just (fromDigits ds)
    else Nothing
    where
        ds = mapMaybe getDigit xs
        fromDigits = foldl (\x e -> x*10 + e) 0

type Mapping = [(Char, Char)]

allMappings :: [Mapping]
allMappings = map (`zip` "abcdefg") (permutations "abcdefg")

getC :: Mapping -> Char -> Char
getC m x = head [b | (a,b) <- m, a == x]

remap :: Mapping -> [String] -> [String]
remap m = map (map (getC m))

tryMap :: [String] -> [String] -> Mapping -> Maybe Int
tryMap xs ys m = case map (remap m |> getInt) [xs, ys] of
    [Just a, Just b] -> Just b
    _ -> Nothing

findPossibleMap :: [String] -> [String] -> Maybe Int
findPossibleMap xs ys = head $ filter isJust $ map (tryMap xs ys) allMappings

g :: [([String], [String])] -> Int
g = map solve |> sum
    where
        solve (xs, ys) = case findPossibleMap xs ys of
            Just a -> a
            Nothing -> error $ show xs ++ show ys ++ "No possible wiring found"
-- 
main :: IO ()
main = interact $
    splitOn "\n" |> map parseLine |> (\xs -> [f xs, g xs]) |> map show |> unlines