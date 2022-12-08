prs :: [String] -> [[Int]]
prs m = [[read [x] :: Int | x <- y] | y <- m]


lwr :: Int -> [Int] -> Int
lwr _ [] = 0
lwr n (x:xs)
    | x < n = 1 + (lwr n xs)
    | otherwise = 1

vis :: Int -> Int -> [[Int]] -> Int
vis a o m = product $ map (lwr (m !! a !! o)) [n, s, w, e]
    where
        n = reverse [m !! x !! o | x <- [0..a-1]]
        s = [m !! x !! o | x <- [a+1..98]]
        w = reverse [m !! a !! y | y <- [0..o-1]]
        e = [m !! a !! y | y <- [o+1..98]]

alv :: [[Int]] -> Int
alv m = maximum [vis x y m | x <- [0..98], y <- [0..98]]

sol :: [Char] -> Int
sol = alv . prs . lines

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ sol input