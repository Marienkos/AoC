isin :: Char -> [Char] -> Bool
isin _ [] = False
isin c (x:xs)
    | c == x = True
    | otherwise = isin c xs

substring :: [String] -> String -> Int -> (Int, Bool)
substring _ [] _ = (0, False)
substring [] _ _ = (0, False)
substring (x:xs) y n
    | take (length x) y == x = (n, True)
    | otherwise = substring xs y (n+1)

decode :: [Char] -> [Int]
decode [] = []
decode list@(x:xs)
    | isin x ['1'..'9'] = (read [x] :: Int) : decode xs
    | snd isNumber = (fst isNumber) : decode xs
    | otherwise = decode xs
        where isNumber = substring ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] list 1

value :: [Int] -> Int
value list = (head list) * 10 + last list

solve :: String -> Int
solve = sum . map (value . decode) . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve
