num :: [String] -> [Int]
num [] = []
num ("":xs) = 0 : (num xs)
num (x:xs) = (read x :: Int) : (num xs)

somme :: [Int] -> [Int]
somme [] = []
somme (0:xs) = somme xs
somme x = (sum $ takeWhile (/= 0) x) : (somme $ dropWhile (/= 0) x)

senza :: [Int] -> Int -> [Int]
senza l m = [x | x <- l, x /= m]

top :: [Int] -> Int
top x = sum [maximum x, maximum $ senza x (maximum x), maximum $ senza (senza x (maximum x)) (maximum $ senza x (maximum x))]

sol :: [Char] -> Int
sol = top . somme . num . lines

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ sol input
