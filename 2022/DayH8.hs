parse :: [String] -> [[Int]]
parse m = [[read [x] :: Int | x <- y] | y <- m]


lower :: Int -> [Int] -> Int
lower _ [] = 0
lower n (x:xs)
    | x < n = 1 + (lower n xs)
    | otherwise = 1

visible :: Int -> Int -> [[Int]] -> Int
visible a o m = product $ map (lower (m !! a !! o)) [n, s, w, e]
    where
        n = reverse [m !! x !! o | x <- [0..a-1]]
        s = [m !! x !! o | x <- [a+1..98]]
        w = reverse [m !! a !! y | y <- [0..o-1]]
        e = [m !! a !! y | y <- [o+1..98]]

allvisible :: [[Int]] -> Int
allvisible m = maximum [visible x y m | x <- [0..98], y <- [0..98]]

solve :: [Char] -> Int
solve = allvisible . parse . lines

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ solve input
