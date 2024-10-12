decode :: [Char] -> [Int]
decode x
    | elem ',' x = read (takeWhile (/= ',') x) : decode (tail $ dropWhile (/= ',') x)
    | otherwise = [read x]

symmetry :: Int -> Int -> Int
symmetry a b
    | a > b = b + 2 * (a-b)
    | otherwise = b

solution :: [Int] -> Int -> Int
solution l n = sum (map (symmetry n) l) - n * length l

solve :: [Char] -> Int
solve l = minimum $ map (solution $ decode l) [minimum (decode l)..maximum (decode l)]

main :: IO ()
main = readFile "input.txt" >>= print . solve