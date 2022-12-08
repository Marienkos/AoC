cmn :: [String] -> [String]
cmn m = [[m !! x !! y | x <- [0..7], m !! x !! y /= ' '] | y <- [1, 5, 9, 13, 17, 21, 25, 29, 33]]

mov :: Int -> Int -> Int -> [[Char]] -> [[Char]]
mov n s z m
    | s <= z = prima ++ da ++ mezzo ++ verso ++ dopo
    | z < s = prima ++ verso ++ mezzo ++ da ++ dopo
        where
            da = [drop n (m !! s)]
            verso = [(take n (m !! s)) ++ (m !! z)]
            prima = take (min s z) m
            mezzo = drop (min s z + 1) (take (max s z) m)
            dopo = drop (max s z + 1) m

num :: [Char] -> [Int]
num l
    | length numeri == 3 = [numeri !! 0, (numeri !! 1)-1, (numeri !! 2)-1]
    | otherwise = [(numeri !! 0)*10 + (numeri !! 1), (numeri !! 2)-1, (numeri !! 3)-1] 
        where numeri = [read [x] :: Int | x <- l, elem x ['0'..'9']]

nll :: [String] -> [[Int]]
nll m = [num x | x <- [m !! y | y <- [10..(length m)-1]]]

exe :: [[Char]] -> Int -> [[Char]]
exe x (-1) = cmn x
exe x y = mov ((nll x) !! y !! 0) ((nll x) !! y !! 1) ((nll x) !! y !! 2) (exe x (y-1))

sol :: [Char] -> [Char]
sol x = map head (exe (lines x) ((length $ nll $ lines x)-1))

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ sol input