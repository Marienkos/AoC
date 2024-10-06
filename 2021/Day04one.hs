split :: Int -> [[Int]] -> [(Int, [Int])]
split _ [] = []
split n (x:xs)
    | null x = split (n+1) xs
    | otherwise = (n, x) : split n xs

splitComma :: [Char] -> [[Char]]
splitComma [] = []
splitComma x = takeWhile (/= ',') x : splitComma (drop 1 (dropWhile (/= ',') x))

mark :: Int -> Int -> Int
mark n m
    | n == m = -1
    | otherwise = m

markAll :: Int -> [(Int, [Int])] -> [(Int, [Int])]
markAll _ [] = []
markAll n ((x1, x2):xs) = (x1, map (mark n) x2) : markAll n xs

check :: [Int] -> Bool
check x = length x == length (filter (== -1) x)

checkRows :: [(Int, [Int])] -> Int
checkRows [] = 0
checkRows ((x1, x2):xs)
    | check x2 = x1
    | otherwise = checkRows xs

checkColumns :: [Int] -> [(Int, [Int])] -> Int
checkColumns [] _ = 0
checkColumns (n:ns) x
    | check $ map ((!! n) . snd) x = fst $ head x
    | otherwise = checkColumns ns x

checkAllColumns :: [Int] -> [(Int, [Int])] -> Int
checkAllColumns [] _ = 0
checkAllColumns (n:ns) x
    | checkColumns [0..4] (filter ((== n) . fst) x) /= 0 = checkColumns [0..4] (filter ((== n) . fst) x)
    | otherwise = checkAllColumns ns x

continue :: [Int] -> [(Int, [Int])] -> (Int, Int)
continue (x:xs) y
    | checkRows (markAll x y) /= 0 = (x, checkRows (markAll x y))
    | checkAllColumns [1..(fst (last y))] (markAll x y) /= 0 = (x, checkAllColumns [1..(fst (last y))] (markAll x y))
    | otherwise = continue xs (markAll x y)

finish :: [Int] -> (Int, Int) -> [(Int, [Int])] -> Int
finish l (a, b) x = a * sum (filter (not . flip elem ((takeWhile (/= a) l)++[a])) (concatMap snd (filter ((== b) . fst) x)))


solve x = finish numbers (continue numbers boards) boards
    where
        boards = split 0 $ map (map read . words) $ tail $ lines x
        numbers = map read $ splitComma $ head $ lines x

main :: IO ()
main = readFile "input.txt" >>= print . solve

-- perché ignora 23?
-- errore con elem: non guardare gli elementi successivi!