split :: [Char] -> (String, String)
split x = (takeWhile (/= ',') x, tail $ dropWhile (/= ',') x)

intersect :: [Int] -> [Int] -> Int
intersect x y
    | length [e | e <- x, elem e x, elem e y] > 0 = 1
    | otherwise = 0

evaluate :: (String, String) -> Int
evaluate (x, y) = intersect (int x) (int y)
    where int x = [(read (takeWhile (/= '-') x) :: Int)..(read (tail $ dropWhile (/= '-') x) :: Int)]

solve :: [Char] -> Int
solve l = sum [evaluate $ split x | x <- lines l]

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ solve input
