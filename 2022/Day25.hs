parse :: [Char] -> [Int]
parse [] = []
parse (x:xs) = case x of
    '1' -> 1 : parse xs
    '2' -> 2 : parse xs
    '=' -> -2 : parse xs
    '-' -> -1 : parse xs
    '0' -> 0 : parse xs

decimal :: [Char] -> Int
decimal = sum . zipWith (*) [5^n | n <- [0..]] . reverse . parse

snafu :: Int -> [Char]
snafu 0 = []
snafu x = case mod x 5 of
    0 -> '0' : snafu (div x 5)
    1 -> '1' : snafu (div x 5)
    2 -> '2' : snafu (div x 5)
    3 -> '=' : snafu (div x 5 + 1)
    4 -> '-' : snafu (div x 5 + 1)

solve :: [Char] -> [Char]
solve = reverse . snafu . sum . map decimal . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve
