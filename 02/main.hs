(|>) :: (a -> b) -> (b -> c) -> a -> c
f |> g = g . f

data CMD  = Forward Int | Up Int | Down Int

parse :: [String] -> CMD
parse (cmd:y:_) = case cmd of
    "forward" -> Forward x
    "up" -> Up x
    "down" -> Down x
    _ -> error "Invalid Input"
    where x = read y

parse _ = error "Invalid Input"

f :: [CMD] -> Int
f = foldl get (0,0) |> uncurry (*)
    where
        get (pos, depth) cmd = case cmd of 
            Forward x -> (pos + x, depth)
            Up x -> (pos, depth - x)
            Down x -> (pos, depth + x)

g :: [CMD] -> Int
g = foldl get (0,0,0) |> \(a,b,_) -> a*b
    where
        get (pos, depth, aim) cmd = case cmd of 
            Forward x -> (pos + x, depth + aim*x, aim)
            Up x      -> (pos, depth, aim - x)
            Down x    -> (pos, depth, aim + x)


main :: IO ()
main = interact $ 
    lines |> map (words |> parse) |> (\xs -> [f xs, g xs]) |> map show |> unlines