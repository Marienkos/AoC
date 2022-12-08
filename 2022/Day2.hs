pts :: [Char] -> Int
pts "A X" = 3
pts "A Y" = 4
pts "A Z" = 8
pts "B X" = 1
pts "B Y" = 5
pts "B Z" = 9
pts "C X" = 2
pts "C Y" = 6
pts "C Z" = 7

sol :: [Char] -> Int
sol l = sum [pts x | x <- lines l]

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ sol input
