onlySpace :: String -> Bool
onlySpace x = not (any (/= ' ') x)

split :: [String] -> [[String]]
split [] = []
split x = takeWhile (not . onlySpace) x : split (drop 1 (dropWhile (not . onlySpace) x))

transpose :: [[a]] -> [[a]]
transpose m = [[m!!r!!c | r <- [0..length m - 1]] | c <- [0.. length (head m) - 1]]

collect :: [String] -> (Char, [Int])
collect x = (last $ head x, map read (init (head x) : tail x))

operate :: (Char, [Int]) -> Int
operate (op, ns) = case op of
    '*' -> product ns
    '+' -> sum ns

solve :: String -> Int
solve = sum . map (operate . collect) . split . transpose . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve