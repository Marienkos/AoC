parse :: [Char] -> [Int]
parse [] = []
parse (x:xs) = case x of
    '1' -> 1 : parse xs
    '2' -> 2 : parse xs
    '=' -> -2 : parse xs
    '-' -> -1 : parse xs
    '0' -> 0 : parse xs

decimal :: [Char] -> Int
decimal x = sum $ zipWith (*) (reverse $ parse x) [5^n | n <- [0..]]

number :: [Char] -> Int
number = sum . map decimal . lines

snafu :: Int -> [Char]
snafu 0 = []
snafu x = case mod x 5 of
    0 -> '0' : snafu (div x 5)
    1 -> '1' : snafu (div x 5)
    2 -> '2' : snafu (div x 5)
    3 -> '=' : snafu (div x 5 + 1)
    4 -> '-' : snafu (div x 5 + 1)

solve :: [Char] -> [Char]
solve = reverse . snafu . number

main :: IO ()
main = readFile "./Advent of code 2022/input.txt" >>= print . solve