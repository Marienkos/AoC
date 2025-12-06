decode :: String -> (Char, Int)
decode x = (head x, read (tail x) :: Int)

sign :: Char -> Int
sign 'L' = -1
sign 'R' = 1

rotate :: (Int, Bool) -> (Char, Int) -> (Int, Bool)
rotate (ex, check) (dir, n) = (mod (ex + sign dir * n) 100, ex == 0)

solve :: String -> Int
solve = length . filter snd . scanl rotate (50, False) . map decode . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve
