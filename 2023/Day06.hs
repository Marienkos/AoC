split :: Char -> String -> [String]
split _ [] = []
split c x = takeWhile (/= c) x : split c (drop 1 (dropWhile (/= c) x))

only :: String -> String
only [] = []
only x = case drop 1 (dropWhile (/= ':') x) of
    "" -> x
    _ -> drop 1 (dropWhile (/= ':') x)

ways :: Int -> Int -> Int
ways time distance = length [x | x <- [0..time], x*(time-x) > distance]

allWays :: [String] -> [String] -> [Int]
allWays [] [] = []
allWays (x:xs) (y:ys) = ways (read x :: Int) (read y :: Int) : allWays xs ys

innerAllWays :: [[String]] -> Int
innerAllWays x = product (allWays (head x) (last x))

main :: IO ()
main = readFile "input.txt" >>= print . innerAllWays . map (filter (/= "") . split ' ' . only) . lines