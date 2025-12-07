is :: Int -> Int -> [[Char]] -> Bool
is r c x = x!!r!!c == '@'

mr :: [[Char]] -> Int
mr x = length x - 1

mc :: [[Char]] -> Int
mc x = length (head x) - 1

neighbors :: Int -> Int -> [[Char]] -> Int
neighbors r c x = sum $ map fromEnum
    [r > 0    &&  c > 0    && is (r-1) (c-1) x,
     r > 0                 && is (r-1) c     x,
     r > 0    &&  c < mc x && is (r-1) (c+1) x,
                  c > 0    && is r     (c-1) x,
                  c < mc x && is r     (c+1) x,
     r < mr x &&  c > 0    && is (r+1) (c-1) x,
     r < mr x              && is (r+1) c     x,
     r < mr x &&  c < mc x && is (r+1) (c+1) x]

forklifts :: [[Char]] -> Int
forklifts x = length [(r, c) | r <- [0..mr x], c <- [0..mc x], is r c x, neighbors r c x < 4]

numMtx :: [[Char]] -> [[(Char, Int, Int)]]
numMtx x = [[(x!!r!!c, r, c) | c <- [0..mc x]] | r <- [0..mr x]]

newPoint :: [[Char]] -> (Char, Int, Int) -> Char
newPoint x (car, r, c)
    | car == '.' = '.'
    | neighbors r c x < 4 = '.'
    | otherwise = '@'

newMtx :: [[Char]] -> [[Char]]
newMtx x = [[newPoint x p | p <- y] | y <- numMtx x]

rolls :: Int -> [[Char]] -> Int
rolls n x
    | forklifts x == 0 = n
    | otherwise = rolls (n + forklifts x) (newMtx x)

solve :: String -> Int
solve = rolls 0 . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve