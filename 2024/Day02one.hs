isIncrementing :: [Int] -> Bool
isIncrementing [_] = True
isIncrementing (a:b:cs)
    | b /= a+1 && b /= a+2 && b /= a+3 = False
    | otherwise = isIncrementing (b:cs)

isDecrementing :: [Int] -> Bool
isDecrementing [_] = True
isDecrementing (a:b:cs)
    | b /= a-1 && b /= a-2 && b /= a-3 = False
    | otherwise = isDecrementing (b:cs)

safe :: [Int] -> Int
safe x
    | isIncrementing x = 1
    | isDecrementing x = 1
    | otherwise = 0

solve :: [Char] -> Int
solve = sum . map (safe . map read . words) . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve