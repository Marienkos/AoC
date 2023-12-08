split :: Char -> String -> [String]
split _ [] = []
split c x = takeWhile (/= c) x : split c (drop 1 (dropWhile (/= c) x))

only :: String -> String
only [] = []
only x = case drop 1 (dropWhile (/= ':') x) of
    "" -> x
    _ -> drop 1 (dropWhile (/= ':') x)

power :: Int -> Int -> Int -> [[String]] -> Int
power r g b [] = r*g*b
power r g b (x:xs)
    | last x == "red" = power (maximum [r, read (head x) :: Int]) g b xs
    | last x == "green" = power r (maximum [g, read (head x) :: Int]) b xs
    | last x == "blue" = power r g (maximum [b, read (head x) :: Int]) xs

solve :: String -> Int
solve = sum . map (power 0 0 0 . map words . concatMap (split ',') . split ';' . only) . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve
