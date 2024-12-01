decode :: ([Int], [Int]) -> [[Char]] -> ([Int], [Int])
decode x [] = x
decode (l, r) (x:xs) = decode (read (head $ words x) : l, read (last $ words x) : r) xs

removeMin :: [Int] -> [Int]
removeMin a@(x:xs)
    | x == minimum a = xs
    | otherwise = x : removeMin xs

diff :: ([Int], [Int]) -> Int
diff ([], []) = 0
diff (l, r) = abs (minimum l - minimum r) + diff (removeMin l, removeMin r)

solve :: [Char] -> Int
solve = diff . decode ([], []) . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve
