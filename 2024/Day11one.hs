figures :: Int -> Int -> Int
figures x n
    | div x 10 > 0 = figures (div x 10) (n+1)
    | otherwise = n

blink :: [Int] -> [Int]
blink [] = []
blink (x:xs)
    | x == 0 = 1 : blink xs
    | mod (figures x 1) 2 == 0 = (read $ take (div (figures x 1) 2) $ show x) : (read $ drop (div (figures x 1) 2) $ show x) : blink xs
    | otherwise = (x*2024) : blink xs

solve :: [Char] -> Int
solve = length . (!! 25) . iterate blink . map read . words

main :: IO ()
main = readFile "input.txt" >>= print . solve