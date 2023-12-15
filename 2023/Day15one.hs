split :: String -> [String]
split [] = []
split x = takeWhile (/= ',') x : split (drop 1 (dropWhile (/= ',') x))

decode :: Int -> String -> Int
decode n [] = n
decode n (x:xs) = decode (((n + (fromEnum x))*17) `mod` 256) xs

solve :: String -> Int
solve = sum . map (decode 0) . split . init

main :: IO ()
main = readFile "input.txt" >>= print . solve