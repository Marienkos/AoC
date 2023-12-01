convert :: [(Char, String)] -> String -> [Int]
convert _ [] = []
convert [] _ = []
convert (x:xs) y
    | fst x == head y || snd x == take (length (snd x)) y = [read [fst x] :: Int]
    | otherwise = substring xs y

decode :: [Char] -> [Int]
decode [] = []
decode (x:xs) = convert [('1', "one"), ('2', "two"), ('3', "three"), ('4', "four"), ('5', "five"), ('6', "six"), ('7', "seven"), ('8', "eight"), ('9', "nine")] (x:xs) ++ decode xs

value :: [Int] -> Int
value list = (head list) * 10 + last list

solve :: String -> Int
solve = sum . map (value . decode) . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve
