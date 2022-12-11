prs :: Int -> (Int, Int) -> [String] -> [(Int, Int)]
prs _ _ [] = []
prs 0 p (l:ls) = prs (read (last $ words $ head ls) :: Int) p ls 
prs d (x, y) (l:ls) = case l !! 0 of
    'U' -> (x, y) : (prs (d-1) (x, y-1) (l:ls))
    'D' -> (x, y) : (prs (d-1) (x, y+1) (l:ls))
    'L' -> (x, y) : (prs (d-1) (x-1, y) (l:ls))
    'R' -> (x, y) : (prs (d-1) (x+1, y) (l:ls))

mov :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
mov _ [] = []
mov (tx, ty) ((px, py) : ls)
    | and [dx <= 1, dy <= 1] = (tx, ty) : (mov (tx, ty) ls)
    | and [dx == 1, dy == 2] = (tx, ty) : (mov (hx, py) ls)
    | and [dx == 2, dy == 1] = (tx, ty) : (mov (px, hy) ls)
    | otherwise = (tx, ty) : (mov (div (tx + hx) 2, div (ty + hy) 2) ls)
        where
            (dx, dy) = (abs (tx-hx), abs (ty-hy))
            (hx, hy) = case ls of
                           [] -> (0, 0)
                           (x:_) -> x

dup :: Eq a => [a] -> [a]
dup [] = []
dup (x:xs)
    | elem x xs = dup xs
    | otherwise = x : (dup xs)

sol :: [Char] -> Int
sol x = length $ dup $ c9
    where
        teste = prs 0 (0, 0) (" " : (lines x))
        c1 = mov (0, 0) teste
        c2 = mov (0, 0) c1
        c3 = mov (0, 0) c2  
        c4 = mov (0, 0) c3
        c5 = mov (0, 0) c4
        c6 = mov (0, 0) c5
        c7 = mov (0, 0) c6
        c8 = mov (0, 0) c7
        c9 = mov (0, 0) c8

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ sol input
