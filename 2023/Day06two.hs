only :: String -> String
only [] = []
only x = case drop 1 (dropWhile (/= ':') x) of
    "" -> x
    _ -> drop 1 (dropWhile (/= ':') x)

ways :: Int -> Int -> Int
ways time distance = time - (length [x | x <- reverse [0..time], x*(time-x) < distance]) + 1

innerWays :: [String] -> Int
innerWays x = ways (read (head x) :: Int) (read (last x) :: Int)

solve :: String -> Int
solve = innerWays . map (concat . words . only) . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve
