allInts :: [Char] -> Bool
allInts x = x == filter (flip elem ['0'..'9']) x

match :: [Char] -> Int
match x
    | length x < 4 = 0
match (a:b:c:d:es)
    | [a, b, c, d] /= "mul(" = 0
    | length (filter (== ',') es) /= 1 = 0
    | not $ allInts first = 0
    | not $ allInts second = 0
    | otherwise = read first * read second
        where
            first = takeWhile (/= ',') es
            second = tail $ dropWhile (/= ',') es


solve :: [Char] -> Int
solve [] = 0
solve a@(x:xs) = match (takeWhile (/= ')') a) + solve xs

main :: IO ()
main = readFile "input.txt" >>= print . solve