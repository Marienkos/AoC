solve :: [Char] -> Int
solve = length . concatMap (filter (`elem` [2, 3, 4, 7]) . map length . take 4 . reverse . words) . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve