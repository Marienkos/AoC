isin :: String -> [String] -> Bool
isin _ [] = False
isin s (x:xs)
    | s == x = True
    | otherwise = isin s xs

split :: Char -> String -> [String]
split _ [] = []
split c x = takeWhile (/= c) x : split c (drop 1 (dropWhile (/= c) x))

only :: String -> String
only [] = []
only x = case drop 1 (dropWhile (/= ':') x) of
    "" -> x
    _ -> drop 1 (dropWhile (/= ':') x)

equal :: [[String]] -> (Int, Int)
equal x = (length [n | n <- head x, isin n (last x)], 1)

add :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
add 0 _ l = l
add n m (x:xs) = (fst x, (snd x) + m) : add (n-1) m xs

addfst :: Int -> [(Int, Int)] -> [(Int, Int)]
addfst 0 (x:xs) = x : add (fst x) (snd x) xs
addfst n (x:xs) = x : addfst (n-1) xs

addall :: Int -> [(Int, Int)] -> [(Int, Int)]
addall n x
    | n == 0 = addfst 0 x
    | otherwise = addfst n $ addall (n-1) x

autoadd :: [(Int, Int)] -> [(Int, Int)]
autoadd x = addall ((length x)-1) x

solve :: String -> Int
solve = sum . map snd .autoadd . map (equal . map (filter (/= "") . split ' ') . split '|'. only) . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve
