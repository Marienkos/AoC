check :: Int -> Int -> [[Char]] -> Bool
check r c m = or
    [r > 0 && elem (m!!(r-1)!!c) string,
     r < (length m)-1 && elem (m!!(r+1)!!c) string,
     c > 0 && elem (m!!r!!(c-1)) string,
     c < (length (head m)-1) && elem (m!!r!!(c+1)) string,
     r > 0 && c > 0 && elem (m!!(r-1)!!(c-1)) string,
     r > 0 && c < (length (head m)-1) && elem (m!!(r-1)!!(c+1)) string,
     r < (length m)-1 && c > 0 && elem (m!!(r+1)!!(c-1)) string,
     r < (length m)-1 && c < (length (head m)-1) && elem (m!!(r+1)!!(c+1)) string]
        where string = "*#+$&@-%$/()'=^!"

reversElem :: Eq a => [a] -> a -> Bool
reversElem a b = elem b a

sep :: String -> [String]
sep [] = []
sep s
    | not $ elem (head s) "1234567890" = takeWhile (not. reversElem "1234567890") s : sep (dropWhile (not. reversElem "1234567890") s)
    | otherwise = takeWhile (reversElem "1234567890") s : sep (dropWhile (reversElem "1234567890") s)

number :: Int -> Int -> [String] -> String
number _ _ [] = []
number now limit (l:ls)
    | now > limit = l
    | otherwise = number (now + (length $ head ls)) limit ls

checkedNum :: Int -> Int -> [[Char]] -> String
checkedNum r c m
    | check r c m = number (length $ head $ sep (m!!r)) c (sep (m!!r))
    | otherwise = []

notAnymore :: String -> Int
notAnymore s = case elem (head s) "1234567890" of
    True -> length $ takeWhile (reversElem "1234567890") s
    False -> length $ takeWhile (not. reversElem "1234567890") s

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