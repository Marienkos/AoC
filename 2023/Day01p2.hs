isin :: Char -> [Char] -> Bool
isin _ [] = False
isin c (x:xs)
    | c == x = True
    | otherwise = isin c xs

substring :: [String] -> String -> Int -> Int
substring _ [] _ = 0
substring [] _ _ = 0
substring (x:xs) y n
    | take (length x) y == x = n
    | otherwise = substring xs y (n+1)

decode :: [Char] -> [Int]
decode [] = []
decode (x:xs)
    | isin x ['1'..'9'] = (read [x] :: Int) : decode xs
    | isNumber /= 0 = isNumber : decode xs
    | otherwise = decode xs
        where isNumber = substring ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] (x:xs) 1

value :: [Int] -> Int
value list = (head list) * 10 + last list

solve :: String -> Int
solve = sum . map (value . decode) . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve
