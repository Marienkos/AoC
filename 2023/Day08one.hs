isin :: Char -> [Char] -> Bool
isin _ [] = False
isin c (x:xs)
    | c == x = True
    | otherwise = isin c xs

split :: String -> [String]
split [] = []
split x = takeWhile (/= ' ') x : split (drop 1 (dropWhile (/= ' ') x))

letters :: String -> String
letters s = filter (/= "") $ split $ [x | x <- s, isin x ['A'..'Z'] || x == ' ']

search :: String -> [[String]] -> (String, String)
search _ [] = ("", "")
search s (x:xs)
    | head x == s = (x!!1, x!!2)
    | otherwise = search s xs

direction :: Int -> String -> [Char] -> [[String]] -> Int
direction n "ZZZ" _ _ = n
direction n s ('L':cs) x = direction (n+1) (fst $ search s x) cs x
direction n s ('R':cs) x = direction (n+1) (snd $ search s x) cs x

solve :: [String] -> Int
solve x = arrive 0 "AAA" (concat [head x | n <- [1..]]) $ map letters $ drop 2 x

main :: IO ()
main = readFile "input.txt" >>= print . solve . lines
