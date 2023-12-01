higher :: [Int] -> [Bool]
higher [b] = []
higher (a:b:c)
    | b > a = True : higher (b:c)
    | otherwise = higher (b:c)

solve :: [Char] -> Int
solve = length . higher . map read . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve
