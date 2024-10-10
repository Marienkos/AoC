initial :: [Char] -> [Int]
initial x = [read [x!!n] | n <- [0, 2..length x - 1]]

nextState :: [Int] -> [Int]
nextState [] = []
nextState (x:xs)
    | x > 0 = x-1 : nextState xs
    | otherwise = 6 : 8 : nextState xs

reApply :: ([Int] -> [Int]) -> Int -> [Int] -> [Int]
reApply _ 0 x = x
reApply f n x = reApply f (n-1) (f x)

solve :: [Char] -> Int
solve = length . reApply nextState 80 . initial

main :: IO ()
main = readFile "input.txt" >>= print . solve