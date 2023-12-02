split :: Char -> String -> [String]
split _ [] = []
split c x = takeWhile (/= c) x : split c (drop 1 (dropWhile (/= c) x))

only :: String -> String
only [] = []
only x = case drop 1 (dropWhile (/= ':') x) of
    "" -> x
    _ -> drop 1 (dropWhile (/= ':') x)

retrieve :: (Int, Int, Int) -> [[String]] -> (Int, Int, Int)
retrieve colors [] = colors
retrieve (r, g, b) (x:xs)
    | last x == "red" = retrieve (maximum [r, read (head x) :: Int], g, b) xs
    | last x == "green" = retrieve (r, maximum [g, read (head x) :: Int], b) xs
    | last x == "blue" = retrieve (r, g, maximum [b, read (head x) :: Int]) xs

fits :: (Int, Int, Int) -> Bool
fits (r, g, b)
    | and [r < 13, g < 14, b < 15] = True
    | otherwise = False

index :: Int -> [(Int, Int, Int)] -> [Int]
index _ [] = []
index n (x:xs)
    | fits x = n : index (n+1) xs
    | otherwise = index (n+1) xs

solve :: String -> Int
solve = sum . index 1 . map (retrieve (0, 0, 0) . map (filter (/= "") . split ' ') . concat . map (split ',') . split ';' . only) . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve