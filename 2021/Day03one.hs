common :: Int -> [[Char]] -> [Int]
common n x
    | n == length (head x) = []
    | l > length x - l = 1 : common (n+1) x
    | otherwise = 0 : common (n+1) x
        where l = sum [read [y!!n] | y <- x]

least :: [[Char]] -> [Int]
least = map (abs . (1-)) . common 0

bin :: [Int] -> Int
bin = sum . zipWith (*) [2^n | n <- [0..]] . reverse

solve :: [Char] -> Int
solve x = bin (least $ lines x) * bin (common 0 $ lines x)

main :: IO ()
main = readFile "input.txt" >>= print . solve