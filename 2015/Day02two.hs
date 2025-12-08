split :: Char -> String -> [String]
split c [] = []
split c x = takeWhile (/= c) x : split c (drop 1 (dropWhile (/= c) x))

decode :: String -> Int
decode s = l*w*h + 2 * minimum [l+w, w+h, h+l]
    where
        splitted = map read $ split 'x' s
        l = splitted !! 0
        w = splitted !! 1
        h = splitted !! 2

solve :: String -> Int
solve = sum . map decode . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve