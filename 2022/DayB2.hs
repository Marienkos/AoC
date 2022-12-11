points :: [Char] -> Int
points "A X" = 3
points "A Y" = 4
points "A Z" = 8
points "B X" = 1
points "B Y" = 5
points "B Z" = 9
points "C X" = 2
points "C Y" = 6
points "C Z" = 7

solve :: [Char] -> Int
solve l = sum [points x | x <- lines l]

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ solve input
