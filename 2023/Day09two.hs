diff :: [Int] -> [Int]
diff (x:[]) = []
diff (x1:x2:xs) = (x1-x2) : diff (x2:xs)

recDiff :: [Int] -> [Int] -> ([Int], [Int])
recDiff acc l
    | l == [n | n <- l, n == 0] = (acc, l++[0])
    | otherwise = recDiff (acc++[head l]) (diff l)

gen :: ([Int], [Int]) -> ([Int], [Int])
gen (acc, l) = (init acc, scanl (-) (last acc) l)

genAll :: ([Int], [Int]) -> Int
genAll ([], l) = last l
genAll (acc, l) = genAll (gen (acc, l))

solve :: String -> Int
solve = sum . map (genAll . recDiff [] . reverse . map (read :: String -> Int) . words) . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve