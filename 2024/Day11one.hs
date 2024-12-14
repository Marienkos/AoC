blink :: [Int] -> [Int]
blink [] = []
blink (x:xs)
    | x == 0 = 1 : blink xs
    | mod (figures x) 2 == 0 = (div x (tens x)) : (mod x (tens x)) : blink xs
    | otherwise = (x*2024) : blink xs
        where
            figures = length . show
            tens = (^) 10 . flip div 2 . figures

solve :: [Char] -> Int
solve = length . (!! 25) . iterate blink . map read . words

main :: IO ()
main = readFile "input.txt" >>= print . solve
