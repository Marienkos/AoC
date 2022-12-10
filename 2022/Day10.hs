prs :: [String] -> [Int]
prs [] = []
prs (x:xs)
    | (words x) !! 0 == "noop" = 0 : (prs xs)
    | otherwise = [0, read ((words x) !! 1) :: Int] ++ (prs xs)

val :: [Char] -> [Int]
val x = [1 + sum [(prs $ lines x) !! n | n <- [0..c-2]] | c <- [1..240]]

drw :: Int -> [Int] -> [Char]
drw _ [] = []
drw n (x:xs)
    | n == 40 = '\n' : (drw 0 (x:xs))
    | elem n [x-1, x, x+1] = '#' : (drw (n+1) xs)
    | otherwise = '.' : (drw (n+1) xs)

sol :: [Char] -> [Char]
sol x = drw 0 (val x)

main :: IO ()
main = do
    input <- readFile "input.txt"
    putStrLn $ sol input