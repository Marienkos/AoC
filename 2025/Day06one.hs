split :: Char -> String -> [String]
split c [] = []
split c x = takeWhile (/= c) x : split c (drop 1 (dropWhile (/= c) x))

transpose :: [[a]] -> [[a]]
transpose m = [[m!!r!!c | r <- [0..length m - 1]] | c <- [0.. length (head m) - 1]]

operate :: [String] -> Int
operate x = case last x of
    "*" -> product $ map read $ init x
    "+" -> sum $ map read $ init x

solve :: String -> Int
solve = sum . map operate . transpose . map (filter (/= "") . split ' ') . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve