sep :: [Char] -> (String, String)
sep x = (takeWhile (/= ',') x, tail $ dropWhile (/= ',') x)

allz :: [Int] -> [Int] -> Int
allz x y
    | length [e | e <- x, elem e x, elem e y] > 0 = 1
    | otherwise = 0

val :: (String, String) -> Int
val (x, y) = allz (int x) (int y)
    where int x = [(read (takeWhile (/= '-') x) :: Int)..(read (tail $ dropWhile (/= '-') x) :: Int)]

sol :: [Char] -> Int
sol l = sum [val $ sep x | x <- lines l]

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ sol input
