(|>) :: (a -> b) -> (b -> c) -> a -> c
f |> g = g . f

inv :: String -> String 
inv = foldr (\ x -> (:) (if x == '1' then '0' else '1')) ""

readBin :: String -> Int
readBin = foldl (\acc x -> acc * 2 + if x == '0' then 0 else 1) 0

f :: [String] -> Int
f xs = readBin gamma * readBin epsilon
    where
        ys = map (map (\b -> if b == '0' then 0 else 1)) xs
        half = div (length ys) 2
        oneCount = foldl (zipWith (+)) [0 | _ <- [1.. length (head ys)]] ys
        gamma = map (\c -> if c > half then '1' else '0') oneCount
        epsilon = inv gamma

data BinTrie =   End | Node Int Int BinTrie BinTrie 
    deriving (Show, Eq) -- Total Count, One Count, Left 0, Right 1

binToTrie  :: String -> BinTrie
binToTrie [] = End
binToTrie (x:xs) = case x of
    '0' -> Node 1 0 (binToTrie xs) End
    '1' -> Node 1 1 End (binToTrie xs)
    _ -> error "Non Binary Input"

insertBin :: BinTrie -> String -> BinTrie
insertBin End [] = End
insertBin _ [] = error "Invalid Input, size < k bits"
insertBin trie (x:xs) = case x of
    '0' -> case trie of
            Node tc oc End right -> Node (tc+1) oc (binToTrie xs) right
            Node tc oc t right   -> Node (tc+1) oc (insertBin t xs) right
            _ -> error "Invalid Input, size > k bits"
    '1' -> case trie of
            Node tc oc left End -> Node (tc+1) (oc+1) left (binToTrie xs)
            Node tc oc left t   -> Node (tc+1) (oc+1) left (insertBin t xs)
            _ -> error "Invalid Input, size > k bits"
    _ -> error "Non Binary Input"

makeKBinTrie :: [String] -> BinTrie
makeKBinTrie (x:xs) = foldl insertBin (binToTrie x) xs
makeKBinTrie [] = End

getMostCommon :: BinTrie -> String
getMostCommon End = ""
getMostCommon (Node tc oc left right)
    | 2*oc >= tc = '1' : getMostCommon right
    | otherwise  = '0' : getMostCommon left

getLeastCommon :: BinTrie -> String
getLeastCommon End = ""
getLeastCommon (Node tc oc left right)
    | oc == 0   = '0' : getLeastCommon left
    | oc == tc  = '1' : getLeastCommon right
    | 2*oc < tc = '1' : getLeastCommon right
    | otherwise = '0' : getLeastCommon left

g :: [String] -> Int
g xs = oxygenR * co2R
    where
        k = length (head xs)
        trie = makeKBinTrie xs
        oxygenR = readBin (getMostCommon trie)
        co2R    = readBin (getLeastCommon trie)

main :: IO ()
main = interact $
    lines |>  (\xs -> [f xs, g xs]) |> map show |> unlines