higher :: [Int] -> [Bool]
higher [b] = []
higher (a:b:c)
    | b > a = True : higher (b:c)
    | otherwise = higher (b:c)

solve :: [Char] -> Int
solve = length . higher . map read . lines

main :: IO ()
main = readFile "./Advent of code 2021/input.txt" >>= print . solve