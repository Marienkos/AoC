letters :: String -> [String]
letters s = words [x | x <- s, elem x ['A'..'Z'] || elem x ['1'..'9'] || x == ' ']

search :: String -> [[String]] -> (String, String)
search _ [] = ("", "")
search s (x:xs)
    | head x == s = (x!!1, x!!2)
    | otherwise = search s xs

direction :: Int -> String -> [Char] -> [[String]] -> Int
direction n s (c:cs) x
    | last s == 'Z' = n
    | c == 'L' = direction (n+1) (fst $ search s x) cs x
    | c == 'R' = direction (n+1) (snd $ search s x) cs x

endpoint :: [String] -> String -> Int
endpoint x s = direction 0 s (concat [head x | n <- [1..]]) $ map letters $ drop 2 x

isStart :: [String] -> Bool
isStart l
    | last (head l) == 'A' = True
    | otherwise = False

startpoint :: [String] -> [String]
startpoint = map head . filter (isStart) . map letters . drop 2

lcmlist :: [Int] -> Int
lcmlist (a:[]) = a
lcmlist (a:b:rest) = lcmlist ((lcm a b):rest)

solve :: [String] -> Int
solve x = lcmlist $ map (endpoint x) (startpoint x)

main :: IO ()
main = readFile "input.txt" >>= print . solve . lines