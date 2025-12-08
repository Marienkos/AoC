deliver :: (Int, Int) -> String -> [(Int, Int)]
deliver a [] = [a]
deliver (h, v) (x1:xs) = case x1 of
    '>' -> (h, v) : deliver (h+1, v) (drop 1 xs)
    '^' -> (h, v) : deliver (h, v+1) (drop 1 xs)
    '<' -> (h, v) : deliver (h-1, v) (drop 1 xs)
    'v' -> (h, v) : deliver (h, v-1) (drop 1 xs)

unique :: [(Int, Int)] -> [(Int, Int)]
unique [] = []
unique (x:xs)
    | elem x xs = unique xs
    | otherwise = x : unique xs


solve x = length $ unique $ deliver (0, 0) (tail x) ++ deliver (0, 0) x

main :: IO ()
main = readFile "input.txt" >>= print . solve