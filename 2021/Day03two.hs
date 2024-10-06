common :: Int -> [[Char]] -> Int
common n x
    | l >= length x - l = 1
    | otherwise = 0
        where l = sum [read [y!!n] | y <- x]

least :: Int -> [[Char]] -> Int
least n x = 1 - common n x

cut :: Int -> Int -> [[Char]] -> [[Char]]
cut n m = filter ((== m) . read . flip (:) [] . (!! n))

cutWhile :: Int -> (Int -> [[Char]] -> Int) -> [[Char]] -> [Char]
cutWhile n f x
    | length x == 1 = head x
    | otherwise = cutWhile (n+1) f (cut n (f n x) x)

bin :: [Int] -> Int
bin = sum . zipWith (*) [2^n | n <- [0..]] . reverse

convert :: (Int -> [[Char]] -> Int) -> [Char] -> Int
convert f = bin . map (read . flip (:) []) . cutWhile 0 f . lines

solve :: [Char] -> Int
solve x = convert common x * convert least x

main :: IO ()
main = readFile "input.txt" >>= print . solve
