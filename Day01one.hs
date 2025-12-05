decode :: String -> (Char, Int)
decode x = (head x, read (tail x) :: Int)

sign :: Char -> Int
sign 'L' = -1
sign 'R' = 1

rotate :: (Int, Int) -> (Char, Int) -> (Int, Int)
rotate (ex, count) (dir, n) = case ex of
    0 -> (new, count + 1)
    _ -> (new, count)
    where new = mod (ex + (sign dir) * n) 100

solve :: String -> Int
solve = snd . foldl rotate (50, 0) . map decode . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve