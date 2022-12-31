parse :: Char -> Int
parse x = case x of
    '1' -> 1
    '2' -> 2
    '=' -> -2
    '-' -> -1
    '0' -> 0
    
snafu :: Int -> [Char]
snafu 0 = []
snafu x = case mod x 5 of
    0 -> '0' : snafu (div x 5)
    1 -> '1' : snafu (div x 5)
    2 -> '2' : snafu (div x 5)
    3 -> '=' : snafu (div x 5  + 1)
    4 -> '-' : snafu (div x 5 + 1)

solve :: [Char] -> [Char]
solve = reverse . snafu . sum . map (sum . zipWith (*) [5^n | n <- [0..]] . reverse . map parse) . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve
