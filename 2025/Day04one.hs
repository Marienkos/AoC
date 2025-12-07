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

solve :: String -> Int
solve = forklifts . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve