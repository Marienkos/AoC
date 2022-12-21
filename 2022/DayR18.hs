split :: String -> [String]
split [] = []
split x = [takeWhile (/= ',') x] ++ (split $ drop 1 (dropWhile (/= ',') x))

parse :: String -> (Int, Int, Int)
parse x = (read (split x !! 0) :: Int, read (split x !! 1) :: Int, read (split x !! 2) :: Int)

sides :: (Int, Int, Int) -> [(Int, Int, Int)] -> Int
sides (x, y, z) lava = 6 - (sum $ map (fromEnum . inlist) [(x, y-1, z), (x, y+1, z), (x-1, y, z), (x+1, y, z), (x, y, z-1), (x, y, z+1)])
    where inlist element = elem element lava

solve :: [Char] -> Int
solve x = sum $ map cube list
    where
        list = map parse (lines x)
        cube x = sides x list

main :: IO ()
main = readFile "input.txt" >>= print . solve
