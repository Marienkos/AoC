parse :: Int -> (Int, Int) -> [String] -> [(Int, Int)]
parse _ _ [] = []
parse 0 p (l:ls) = parse (read (last $ words $ head ls) :: Int) p ls
parse d (x, y) (l:ls) = case head l of
    'U' -> (x, y) : parse (d-1) (x, y-1) (l:ls)
    'D' -> (x, y) : parse (d-1) (x, y+1) (l:ls)
    'L' -> (x, y) : parse (d-1) (x-1, y) (l:ls)
    'R' -> (x, y) : parse (d-1) (x+1, y) (l:ls)

move :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
move _ [] = []
move (tx, ty) ((px, py) : ls @ ((hx, hy):xs))
    | (dx <= 1) && (dy <= 1) = (tx, ty) : move (tx, ty) ls
    | (dx == 1) && (dy == 2) = (tx, ty) : move (hx, py) ls
    | (dx == 2) && (dy == 1) = (tx, ty) : move (px, hy) ls
    | otherwise = (tx, ty) : move (div (tx+hx) 2, div (ty+hy) 2) ls
        where
            (dx, dy) = (abs (tx-hx), abs (ty-hy))

noduplicates :: Eq a => [a] -> [a]
noduplicates [] = []
noduplicates (x:xs)
    | elem x xs = noduplicates xs
    | otherwise = x : noduplicates xs

solve :: [Char] -> Int
solve x = length $ noduplicates c9
    where
        teste = parse 0 (0, 0) (" " : lines x)
        c1 = move (0, 0) teste
        c2 = move (0, 0) c1
        c3 = move (0, 0) c2
        c4 = move (0, 0) c3
        c5 = move (0, 0) c4
        c6 = move (0, 0) c5
        c7 = move (0, 0) c6
        c8 = move (0, 0) c7
        c9 = move (0, 0) c8

main :: IO ()
main = readFile "input.txt" >>= print . solve
