decodeCoor :: [Char] -> (Int, Int)
decodeCoor x = (read $ takeWhile (/= ',') x, read $ tail $ dropWhile (/= ',') x)

decodeIns :: [Char] -> (Char, Int)
decodeIns x = (head x, read $ tail $ dropWhile (/= '=') x)

mirror :: (Char, Int) -> (Int, Int) -> (Int, Int)
mirror ('x', n) (x, y)
    | x <= n = (x, y)
    | otherwise = (-x + 2*n, y)
mirror ('y', n) (x, y)
    | y <= n = (x, y)
    | otherwise = (x, -y + 2*n)

mirrorCoors :: [(Int, Int)] -> [(Char, Int)] -> [(Int, Int)]
mirrorCoors coors [] = coors
mirrorCoors coors (line:ls) = mirrorCoors (map (mirror line) coors) ls

removeDuplicates :: Eq a => [a] -> [a] -> [a]
removeDuplicates [] y = y
removeDuplicates (x:xs) y = case elem x y of
    True -> removeDuplicates xs y
    False -> removeDuplicates xs (x:y)

solve :: [Char] -> [(Int, Int)]
solve x = removeDuplicates (mirrorCoors coors ins) []
    where
        coors = map decodeCoor $ takeWhile (/= "") $ lines x
        ins = map (decodeIns . last . words) $ tail $ dropWhile (/= "") $ lines x

main :: IO ()
main = readFile "input.txt" >>= print . solve

-- I put every single point (x, y) in GeoGebra