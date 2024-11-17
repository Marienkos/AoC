insert :: String -> [(String, String)] -> String
insert pair rules
    | length crp == 1 = [head pair] ++ snd (head crp)
    | otherwise = [head pair]
        where crp = filter (\(x, y) -> x == pair) rules

insertAll :: String -> [(String, String)] -> String
insertAll [] rules = []
insertAll string rules = insert (take 2 string) rules ++ insertAll (tail string) rules

insertN :: String -> [(String, String)] -> Int -> String
insertN string rules 0 = string
insertN string rules n = insertN (insertAll string rules) rules (n-1)

frequency :: String -> Char -> Int
frequency s c = length $ filter (== c) s

solve :: [Char] -> Int
solve x = maximum frequencies - minimum frequencies
    where
        finalString = insertN (head $ lines x) (map (\[a,b,' ','-','>',' ',c] -> ([a,b], [c])) (tail $ tail $ lines x)) 10
        frequencies = filter (/= 0) $ map (frequency finalString) ['A'..'Z']

main :: IO ()
main = readFile "input.txt" >>= print . solve