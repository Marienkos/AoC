partial :: [(Int, Int, Int)] -> [[Int]] -> [(Int, Int, Int)]
partial [] _ = []
partial ((a, b, c):xs) ints
    | rightExists && downExists = right : down : partial xs ints
    | rightExists = right : partial xs ints
    | downExists = down : partial xs ints
    | otherwise = []
    where
        rightExists = b < length ints - 1
        downExists = a < length ints - 1
        right = (a, b+1, c + (ints!!a!!(b+1)))
        down = (a+1, b, c + (ints!!(a+1)!!b))

total :: [(Int, Int, Int)] -> [[Int]] -> Int
total x ints = case partial x ints of
    [] -> minimum $ map (\(a, b, c) -> c) x
    _ -> total (partial x ints) ints

solve :: [Char] -> Int
solve = total [(0, 0, 0)] . map (map (\x -> read [x])) . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve