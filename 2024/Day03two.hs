allInts :: [Char] -> Bool
allInts x = x == filter (flip elem ['0'..'9']) x

matchMul :: [Char] -> Int
matchMul x
    | length x < 4 = 0
matchMul (a:b:c:d:es)
    | [a, b, c, d] /= "mul(" = 0
    | length (filter (== ',') es) /= 1 = 0
    | not $ allInts first = 0
    | not $ allInts second = 0
    | otherwise = read first * read second
        where
            first = takeWhile (/= ',') es
            second = tail $ dropWhile (/= ',') es

solve :: Int -> [Char] -> Int
solve _ [] = 0
solve _ a
    | length a >= 4 && take 4 a == "do()" = solve 1 (drop 4 a)
    | length a >= 7 && take 7 a == "don't()" = solve 0 (drop 7 a)
solve n a@(x:xs) = n * (matchMul (takeWhile (/= ')') a)) + solve n xs

main :: IO ()
main = readFile "input.txt" >>= print . solve 1