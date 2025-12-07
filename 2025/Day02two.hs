split :: Char -> String -> [String]
split c [] = []
split c x = takeWhile (/= c) x : split c (drop 1 (dropWhile (/= c) x))

intervals :: String -> [Int]
intervals s = [(read (head $ split '-' s) :: Int)..(read (last $ split '-' s) :: Int)]

separate :: Int -> String -> [String]
separate _ [] = []
separate n s = take n s : separate n (drop n s)

allEquals :: [String] -> Bool
allEquals x = x == filter (== head x) x 

isRepeated :: Int -> String -> Bool
isRepeated n s
    | length s == n = False
    | allEquals $ separate n s = True
    | otherwise = isRepeated (n+1) s

checkInt :: Int -> Bool
checkInt = isRepeated 1 . show

solve :: String -> Int
solve = sum . concatMap (filter checkInt . intervals) . split ','

main :: IO ()
main = readFile "input.txt" >>= print . solve