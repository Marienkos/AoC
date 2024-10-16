count :: [Char] -> [Char] -> [Char]
count open [] = open
count open (l:ls)
    | elem l "([{<" = count (l:open) ls
count (o:os) (l:ls) = case [o, l] of
    "()" -> count os ls
    "[]" -> count os ls
    "{}" -> count os ls
    "<>" -> count os ls
    _ -> []

points :: Char -> Int
points x = case x of
    '(' -> 1
    '[' -> 2
    '{' -> 3
    '<' -> 4

sumPoints :: [Int] -> Int
sumPoints = foldl (\ n x -> n * 5 + x) 0

sort :: [Int] -> [Int]
sort [] = []
sort a = minimum a : sort [x | x <- a, x /= minimum a]

sorted :: [Char] -> [Int]
sorted = sort . filter (/= 0) . map (sumPoints . map points . count []) . lines

solve :: [Char] -> Int
solve x = sorted x !! div (length $ sorted x) 2

main :: IO ()
main = readFile "input.txt" >>= print . solve