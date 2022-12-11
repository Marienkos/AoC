different :: [Char] -> Bool
different l = length l == length [x | x <- ['a'..'z'], elem x l]

evaluate :: [Char] -> [Int]
evaluate (x:xs)
    | different $ take 14 (x:xs) = [14]
    | otherwise = 1 : (evaluate xs)

solve :: [Char] -> Int
solve = sum . val

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ solve input
