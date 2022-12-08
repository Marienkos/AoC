dif :: [Char] -> Bool
dif l = length l == length [x | x <- ['a'..'z'], elem x l]

val :: [Char] -> [Int]
val (x:xs)
    | dif $ take 14 (x:xs) = [14]
    | otherwise = 1 : (val xs)

sol :: [Char] -> Int
sol = sum . val

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ sol input
