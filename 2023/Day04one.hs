split :: Char -> String -> [String]
split _ [] = []
split c x = takeWhile (/= c) x : split c (drop 1 (dropWhile (/= c) x))

only :: String -> String
only [] = []
only x = case drop 1 (dropWhile (/= ':') x) of
    "" -> x
    _ -> drop 1 (dropWhile (/= ':') x)

equal :: [[String]] -> Int
equal x = length [n | n <- head x, elem n (last x)]

power :: Int -> Int
power 0 = 0
power n = 2^(n-1)

solve :: String -> Int
solve = sum . map (power . equal . map words . split '|'. only) . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve
