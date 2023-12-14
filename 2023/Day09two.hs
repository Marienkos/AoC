diff :: [Int] -> [Int]
diff (x:[]) = []
diff (x1:x2:xs) = (x1-x2) : diff (x2:xs)

recDiff :: [Int] -> [Int] -> ([Int], [Int])
recDiff acc l
    | l == filter (== 0) l = (acc, l++[0])
    | otherwise = recDiff (acc++[head l]) (diff l)

gen :: ([Int], [Int]) -> Int
gen ([], l) = last l
gen (acc, l) = gen (init acc, scanl (-) (last acc) l)

solve :: String -> Int
solve = sum . map (gen . recDiff [] . reverse . map (read :: String -> Int) . words) . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve
