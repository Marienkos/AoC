isin :: Char -> [Char] -> Bool
isin _ [] = False
isin c (x:xs)
    | c == x = True
    | otherwise = isin c xs

split :: String -> [String]
split [] = []
split x = takeWhile (/= ' ') x : split (drop 1 (dropWhile (/= ' ') x))

letters :: String -> String
letters s = [x | x <- s, isin x ['A'..'Z'] || x == ' ']

search :: String -> [[String]] -> (String, String)
search _ [] = ("", "")
search s (x:xs)
    | head x == s = (x!!1, x!!2)
    | otherwise = search s xs

direction :: String -> Char -> [[String]] -> String
direction s 'L' x = fst (search s x)
direction s 'R' x = snd (search s x)

arrive :: Int -> String -> [Char] -> [[String]] -> Int
arrive n "ZZZ" _ _ = n
arrive n s (c:cs) x = arrive (n+1) (direction s c x) cs x

solve :: [String] -> Int
solve x = arrive 0 "AAA" (concat [head x | n <- [1..]]) $ map (filter (/= "") . split . letters) $ drop 2 x

main :: IO ()
main = readFile "input.txt" >>= print . solve . lines
