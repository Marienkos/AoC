split :: Char -> String -> [String]
split c [] = []
split c x = takeWhile (/= c) x : split c (drop 1 (dropWhile (/= c) x))

intervals :: String -> (Int, Int)
intervals s = (read (head $ split '-' s), read (last $ split '-' s))

isRanged :: Int -> (Int, Int) -> Bool
isRanged n (a, b) = n >= a && n <= b

inIntervals :: [(Int, Int)] -> Int -> Bool
inIntervals x n = any (isRanged n) x

solve :: String -> Int
solve x = length $ filter id $ map (inIntervals ranges) ingredients
    where
        ranges = map intervals . takeWhile (/= "") $ lines x
        ingredients = map read $ tail $ dropWhile (/= "") $ lines x

main :: IO ()
main = readFile "input.txt" >>= print . solve