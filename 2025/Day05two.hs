split :: Char -> String -> [String]
split c [] = []
split c x = takeWhile (/= c) x : split c (drop 1 (dropWhile (/= c) x))

intervals :: String -> (Int, Int)
intervals s = (read (head $ split '-' s), read (last $ split '-' s))

isRanged :: Int -> (Int, Int) -> Bool
isRanged n (a, b) = n >= a && n <= b

fallsInto :: Int -> [(Int, Int)] -> (Int, Int)
fallsInto _ [] = (1, 0)
fallsInto n (x:xs)
    | isRanged n x = x
    | otherwise = fallsInto n xs

defInto :: Int -> (Int, Int) -> [(Int, Int)] -> (Int, Int)
defInto n t x
    | fallsInto n filtered == (1, 0) = fallsInto n x
    | otherwise = fallsInto n filtered
        where filtered = filter (/= t) x

inside :: (Int, Int) -> (Int, Int) -> Bool
inside (a, b) (c, d) = isRanged c (a, b) && isRanged d (a, b) 

update :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
update (a, b) x = united : filter (not . inside united) x
    where united = (fst $ defInto a (a, b) x, snd $ defInto b (a, b) x)

newRanges :: [(Int, Int)] -> [(Int, Int)]
newRanges [] = []
newRanges l@(x:xs)
    | update x l == l = x : newRanges xs
    | otherwise = newRanges (update x l)

unfold :: (Int, Int) -> Int
unfold (a, b) = b - a + 1

solve :: String -> Int
solve = sum . map unfold . newRanges . map intervals . takeWhile (/= "") . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve
