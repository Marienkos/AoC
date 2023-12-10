check :: Int -> Int -> [[Char]] -> Bool
check r c m = or
    [r > 0  &&                  elem (m!!(r-1)!!c) string,
     r < rmax &&                elem (m!!(r+1)!!c) string,
                    c > 0 &&    elem (m!!r!!(c-1)) string,
                    c < cmax && elem (m!!r!!(c+1)) string,
     r > 0 &&       c > 0 &&    elem (m!!(r-1)!!(c-1)) string,
     r > 0 &&       c < cmax && elem (m!!(r-1)!!(c+1)) string,
     r < rmax &&    c > 0 &&    elem (m!!(r+1)!!(c-1)) string,
     r < rmax &&    c < cmax && elem (m!!(r+1)!!(c+1)) string]
        where
            string = "*#+$&@-%$/()'=^!"
            rmax = (length m)-1
            cmax = (length (head m)-1)

notAnymore :: String -> Int
notAnymore s = case elem (head s) "1234567890" of
    True -> length $ takeWhile ((flip elem) "1234567890") s
    False -> length $ takeWhile (not . (flip elem) "1234567890") s

sep :: String -> [String]
sep [] = []
sep s = take (notAnymore s) s : sep (drop (notAnymore s) s)

number :: Int -> Int -> [String] -> String
number _ _ [] = []
number now limit (l:ls)
    | now > limit = l
    | otherwise = number (now + (length $ head ls)) limit ls

checkedNum :: Int -> Int -> [[Char]] -> String
checkedNum r c m
    | check r c m = number (length $ head $ sep (m!!r)) c (sep (m!!r))
    | otherwise = []

checkedRow :: Int -> Int -> [[Char]] -> [String]
checkedRow r c m
    | c < length (m!!r) && check r c m = checkedNum r c m : checkedRow r (c+notAnymore (drop c (m!!r))) m
    | c < length (m!!r) = checkedRow r (c+1) m
    | otherwise = []

onlyNumbers :: [[Char]] -> [[String]]
onlyNumbers m = [filter (not . elem '.') (filter (/= "") (checkedRow r 0 m)) | r <- [0..length m-1]]

solve :: String -> Int
solve = sum . map (read :: String -> Int) . concat . filter (/= []) . onlyNumbers . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve
