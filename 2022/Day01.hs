numerate :: [String] -> [Int]
numerate [] = []
numerate ("":xs) = 0 : numerate xs
numerate (x:xs) = (read x :: Int) : numerate xs

somme :: [Int] -> [Int]
somme [] = []
somme (0:xs) = somme xs
somme x = sum (takeWhile (/= 0) x) : somme (dropWhile (/= 0) x)

senza :: [Int] -> Int -> [Int]
senza l m = [x | x <- l, x /= m]

top :: [Int] -> Int
top x = sum [maximum x, maximum $ senza x (maximum x), maximum $ senza (senza x (maximum x)) (maximum $ senza x (maximum x))]

solve :: String -> Int
solve = top . somme . numerate . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve
