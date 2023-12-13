split :: [String] -> [[String]]
split [] = []
split x = takeWhile (/= "") x : split (drop 1 (dropWhile (/= "") x))

hSymmetry :: Int -> [[Char]] -> Bool
hSymmetry n m = before == reverse after
    where
        k = min (n+1) (length (head m) - n - 1)
        before = [map (head . drop c) m | c <- [n-k+1..n]]
        after = [map (head . drop c) m | c <- [n+1..n+k]]

hSymmetryBefore :: Int -> [[Char]] -> Int
hSymmetryBefore n m
    | n == length (head m) - 1 = 0
    | otherwise = case hSymmetry n m of
        True -> n+1
        False -> hSymmetryBefore (n+1) m

vSymmetry :: Int -> [[Char]] -> Bool
vSymmetry n m = before == reverse after
    where
        k = min (n+1) (length m - n - 1)
        before = [m!!r | r <- [n-k+1..n]]
        after = [m!!r | r <- [n+1..n+k]]

vSymmetryAbow :: Int -> [[Char]] -> Int
vSymmetryAbow n m
    | n == length m - 1 = 0
    | otherwise = case vSymmetry n m of
        True -> 100*(n+1)
        False -> vSymmetryAbow (n+1) m

symmetry :: [[Char]] -> Int
symmetry m = hSymmetryBefore 0 m + vSymmetryAbow 0 m

solve :: String -> Int
solve = sum . map symmetry . split . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve