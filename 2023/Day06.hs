split :: Char -> String -> [String]
split _ [] = []
split c x = takeWhile (/= c) x : split c (drop 1 (dropWhile (/= c) x))

only :: String -> String
only [] = []
only x = case drop 1 (dropWhile (/= ':') x) of
    "" -> x
    _ -> drop 1 (dropWhile (/= ':') x)

ways :: Int -> Int -> Int
ways time distance = time - (length [x | x <- reverse [0..time], x*(time-x) < distance]) + 1

innerWays :: [String] -> Int
innerWays x = ways (read (head x) :: Int) (read (last x) :: Int)

main :: IO ()
main = readFile "input.txt" >>= print . innerWays . map (concat . split ' ' . only) . lines