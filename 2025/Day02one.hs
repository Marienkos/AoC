split :: Char -> String -> [String]
split c [] = []
split c x = takeWhile (/= c) x : split c (drop 1 (dropWhile (/= c) x))

intervals :: String -> [Int]
intervals s = [(read (head $ split '-' s) :: Int)..(read (last $ split '-' s) :: Int)]

repTwice :: String -> String -> Bool
repTwice s x
    | length s > length x = False
    | s == x = True
    | otherwise = repTwice (s ++ [head x]) (tail x)

checkInt :: Int -> Bool
checkInt x = repTwice "" (show x)

solve :: String -> Int
solve = sum . concatMap (filter checkInt . intervals) . split ','

main :: IO ()
main = readFile "input.txt" >>= print . solve