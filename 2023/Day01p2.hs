isin :: Char -> [Char] -> Bool
isin _ [] = False
isin c (x:xs)
    | c == x = True
    | otherwise = isin c xs

substring :: String -> String -> Bool
substring _ [] = False
substring x y
    | take (length x) y == x = True
    | otherwise = False

decode :: [Char] -> [Int]
decode [] = []
decode list@(x:xs)
    | isin x ['1'..'9'] = (read [x] :: Int) : decode xs
    | substring "one" list = 1 : decode xs
    | substring "two" list = 2 : decode xs
    | substring "three" list = 3 : decode xs
    | substring "four" list = 4 : decode xs
    | substring "five" list = 5 : decode xs
    | substring "six" list = 6 : decode xs
    | substring "seven" list = 7 : decode xs
    | substring "eight" list = 8 : decode xs
    | substring "nine" list = 9 : decode xs
    | otherwise = decode xs

value :: [Int] -> Int
value list = (head list) * 10 + last list

solve :: String -> Int
solve = sum . map (value . decode) . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve
