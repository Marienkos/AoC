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
    | and [abs (tx-hx) <= 1, abs (ty-hy) <= 1] = (tx, ty) : (mov (tx, ty) ls)
    | otherwise = (tx, ty) : (mov (px, py) ls)
        where (hx, hy) = case ls of
                           [] -> (0, 0)
                           (x:_) -> x

dup :: Eq a => [a] -> [a]
dup [] = []
dup (x:xs)
    | elem x xs = dup xs
    | otherwise = x : (dup xs)

sol :: [Char] -> Int
sol x = length $ dup $ mov (0, 0) (prs 0 (0, 0) (" " : (lines x)))

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ sol input