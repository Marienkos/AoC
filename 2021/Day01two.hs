windows :: [Int] -> [Int]
windows [b, c] = []
windows l = sum (take 3 l) : windows (tail l)

higher :: [Int] -> [Bool]
higher [b] = []
higher (a:b:c)
    | b > a = True : higher (b:c)
    | otherwise = higher (b:c)

solve :: [Char] -> Int
solve = length . higher . windows . map read . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve