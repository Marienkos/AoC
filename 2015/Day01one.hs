solve :: Int -> String -> Int
solve n [] = n
solve n ('(':xs) = solve (n+1) xs
solve n (')':xs) = solve (n-1) xs

main :: IO ()
main = readFile "input.txt" >>= print . solve 0
