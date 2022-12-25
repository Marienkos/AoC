parse :: [String] -> [Int]
parse [] = []
parse (x:xs)
    | head (words x) == "noop" = 0 : parse xs
    | otherwise = [0, read (words x !! 1) :: Int] ++ parse xs

evaluate :: [Char] -> [Int]
evaluate x = [1 + sum [parse (lines x) !! n | n <- [0..c-2]] | c <- [1..240]]

draw :: Int -> [Int] -> [Char]
draw _ [] = []
draw n (x:xs)
    | n == 40 = '\n' : draw 0 (x:xs)
    | elem n [x-1, x, x+1] = '#' : draw (n+1) xs
    | otherwise = '.' : draw (n+1) xs

solve :: [Char] -> [Char]
solve x = draw 0 (evaluate x)

main :: IO ()
main = readFile "input.txt" >>= print . solve
