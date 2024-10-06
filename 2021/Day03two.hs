common :: Int -> [[Char]] -> Int
common n x
    | l >= length x - l = 1
    | otherwise = 0
        where l = sum [read [y!!n] | y <- x]

least :: Int -> [[Char]] -> Int
least n x = 1 - common n x

wrap :: a -> [a]
wrap x = [x]

cut :: Int -> Int -> [[Char]] -> [[Char]]
cut n m x = filter ((== m) . read . wrap . head . drop n) x

cutWhile :: Int -> (Int -> [[Char]] -> Int) -> [[Char]] -> [Char]
cutWhile n f x
    | length x == 1 = head x
    | otherwise = cutWhile (n+1) f (cut n (f n x) x)

bin :: [Int] -> Int
bin = sum . zipWith (*) [2^n | n <- [0..]] . reverse

convert :: (Int -> [[Char]] -> Int) -> [Char] -> Int
convert f x = bin $ map (read . wrap) $ cutWhile 0 f (lines x)

solve :: [Char] -> Int
solve x = convert common x * convert least x

main :: IO ()
main = readFile "input.txt" >>= print . solve