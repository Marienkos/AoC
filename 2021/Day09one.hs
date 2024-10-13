readAll :: [Char] -> [Int]
readAll = map (read . (:[]))

point :: Int -> Int -> [[Int]] -> Int
point r c ns
    | r == -1 || c == -1 || r == length ns || c == length (head ns) = -1
    | otherwise = ns !! r !! c

lowPoint :: Int -> Int -> [[Int]] -> Bool
lowPoint r c ns = not $ any (<= point r c ns) (filter (>= 0) [point (r-1) c ns, point r (c-1) ns, point r (c+1) ns, point (r+1) c ns])


solve :: [Char] -> Int
solve x = sum [succ (point r c input) | r <- [0..length input - 1], c <- [0..length (head input) - 1], lowPoint r c input]
    where input = map readAll $ lines x

main :: IO ()
main = readFile "input.txt" >>= print . solve