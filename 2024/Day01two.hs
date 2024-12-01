decode :: ([Int], [Int]) -> [[Char]] -> ([Int], [Int])
decode x [] = x
decode (l, r) (x:xs) = decode (read (head $ words x) : l, read (last $ words x) : r) xs

sim :: ([Int], [Int]) -> Int
sim ([], _) = 0
sim ((l:ls), r) = l * length (filter (== l) r) + sim (ls, r)

solve :: [Char] -> Int
solve = sim . decode ([], []) . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve