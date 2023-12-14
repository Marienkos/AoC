transpose:: [[Char]] -> [[Char]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

update :: Int -> [Char] -> [(Char, Int)]
update _ [] = []
update n (x:xs) = case x of
    'O' -> (x, n) : update n xs
    '.' -> (x, (n+1)) : update n xs
    '#' -> (x, (n+2)) : update (n+2) xs

sort :: [(Char, Int)] -> [(Char, Int)]
sort [] = []
sort (x:xs) = sort [a | a <- xs, snd a < snd x] ++ [x] ++ sort [b | b <- xs, snd b >= snd x]

load :: [[Char]] -> Int
load l = sum [(length [y | y <- l!!x, y == 'O']) * (length l - x) | x <- [0..length l - 1]]

solve :: String -> Int
solve = load . transpose . map (map fst . sort . update 0) . transpose . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve

{-

....O#.O#.
1111023245
0111122345
O....#O.#.

-}