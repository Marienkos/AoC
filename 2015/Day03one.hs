deliver :: (Int, Int) -> String -> [(Int, Int)]
deliver a [] = [a]
deliver (h, v) (x:xs) = case x of
    '>' -> (h, v) : deliver (h+1, v) xs
    '^' -> (h, v) : deliver (h, v+1) xs
    '<' -> (h, v) : deliver (h-1, v) xs
    'v' -> (h, v) : deliver (h, v-1) xs

unique :: [(Int, Int)] -> [(Int, Int)]
unique [] = []
unique (x:xs)
    | elem x xs = unique xs
    | otherwise = x : unique xs

solve :: String -> Int
solve = length . unique . deliver (0, 0)

main :: IO ()
main = readFile "input.txt" >>= print . solve