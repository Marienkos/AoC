isin :: Char -> [Char] -> Bool
isin _ [] = False
isin c (x:xs)
    | c == x = True
    | otherwise = isin c xs

decode :: [Char] -> [Int]
decode [] = []
decode (x:xs)
    | isin x ['0'..'9'] = (read [x] :: Int) : decode xs
    | otherwise = decode xs

value :: [Int] -> Int
value list = (head list) * 10 + last list

solve :: String -> Int
solve = sum . map (value . decode) . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve