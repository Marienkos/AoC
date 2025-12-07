decode :: [Char] -> [Int]
decode = map (read . (: []))

trueMax :: [Int] -> Int
trueMax x
    | last x == maximum x = maximum $ init x
    | otherwise = maximum x

joltage :: [Int] -> Int
joltage l@(x:xs)
    | x == trueMax l = 10*x + maximum xs
    | otherwise = joltage xs

solve :: String -> Int
solve = sum . map (joltage . decode) . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve