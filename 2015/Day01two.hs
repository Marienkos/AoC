solve :: Int -> Int -> String -> Int
solve (-1) p _ = p
solve f p ('(':xs) = solve (f+1) (p+1) xs
solve f p (')':xs) = solve (f-1) (p+1) xs

main :: IO ()
main = readFile "input.txt" >>= print . solve 0 0