column :: [String] -> [String]
column m = [[m !! x !! y | x <- [0..7], m !! x !! y /= ' '] | y <- [1, 5, 9, 13, 17, 21, 25, 29, 33]]

move :: Int -> Int -> Int -> [[Char]] -> [[Char]]
move n s z m
    | s <= z = prima ++ da ++ mezzo ++ verso ++ dopo
    | z < s = prima ++ verso ++ mezzo ++ da ++ dopo
        where
            da = [drop n (m !! s)]
            verso = [take n (m !! s) ++ (m !! z)]
            prima = take (min s z) m
            mezzo = drop (min s z + 1) (take (max s z) m)
            dopo = drop (max s z + 1) m

numerate :: [Char] -> [Int]
numerate l
    | length numeri == 3 = [head numeri, (numeri !! 1)-1, (numeri !! 2)-1]
    | otherwise = [head numeri*10 + (numeri !! 1), (numeri !! 2)-1, (numeri !! 3)-1]
        where numeri = [read [x] :: Int | x <- l, elem x ['0'..'9']]

nll :: [String] -> [[Int]]
nll m = [numerate x | x <- [m !! y | y <- [10..length m-1]]]

execute :: [[Char]] -> Int -> [[Char]]
execute x (-1) = column x
execute x y = move (head (nll x !! y)) (nll x !! y !! 1) (nll x !! y !! 2) (execute x (y-1))

solve :: [Char] -> [Char]
solve x = map head (execute (lines x) (length (nll $ lines x)-1))

main :: IO ()
main = readFile "input.txt" >>= print . solve
