decode :: String -> (Int, Int)
decode x = (read $ takeWhile (/= ',') x, read $ tail $ dropWhile (/= ',') x)

area :: (Int, Int) -> (Int, Int) -> Int
area (a, b) (c, d) = abs ((c-a+1)*(d-b+1))

largest :: [(Int, Int)] -> Int
largest x = maximum [area a b | a <- x, b <- x]

solve :: String -> Int
solve = largest . map decode . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve