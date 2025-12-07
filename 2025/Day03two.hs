decode :: [Char] -> [Int]
decode = map (read . (: []))

trueMax :: Int -> [Int] -> Int
trueMax n x
    | elem (maximum x) (drop (length x - n) x) = maximum $ take (length x - n) x
    | otherwise = maximum x

joltage :: Int -> [Int] -> Int
joltage (-1) _ = 0
joltage n l@(x:xs)
    | x == trueMax n l = (10^n)*x + joltage (n-1) xs
    | otherwise = joltage n xs

solve :: String -> Int
solve = sum . map (joltage 11 . decode) . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve