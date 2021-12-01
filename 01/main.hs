(|>) :: (a -> b) -> (b -> c) -> a -> c
f |> g = g . f

f :: Int -> [Int] -> Int
f s xs = sum $ map fromEnum (zipWith (<) xs (drop s xs))

main :: IO ()
main = interact $ 
    lines |> map read |> (\xs -> [f 1 xs, f 3 xs]) |> map show |> unlines