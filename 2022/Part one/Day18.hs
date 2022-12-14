split :: String -> [String]
split [] = []
split x = takeWhile (/= ',') x : split (drop 1 (dropWhile (/= ',') x))

parse :: String -> (Int, Int, Int)
parse x = (read (head (split x)) :: Int, read (split x !! 1) :: Int, read (split x !! 2) :: Int)

sides :: (Int, Int, Int) -> [(Int, Int, Int)] -> Int
sides (x, y, z) lava = 6 - sum (map (fromEnum . (`elem` lava)) [(x, y-1, z), (x, y+1, z), (x-1, y, z), (x+1, y, z), (x, y, z-1), (x, y, z+1)])

solve :: [Char] -> Int
solve x = sum $ map (`sides` list) list
    where list = map parse (lines x)

main :: IO ()
main = readFile "input.txt" >>= print . solve
