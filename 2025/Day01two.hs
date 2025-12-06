decode :: String -> (Char, Int)
decode x = (head x, read (tail x) :: Int)

sign :: Char -> Int
sign 'L' = -1
sign 'R' = 1

rotate :: (Int, Int) -> (Char, Int) -> (Int, Int)
rotate (ex, _) (dir, n) = (mod (ex + sign dir * n) 100, div (mod (sign dir * ex) 100 + n) 100)

solve :: String -> Int
solve = sum . map snd . scanl rotate (50, 0) . map decode . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve