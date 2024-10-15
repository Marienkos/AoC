count :: [Char] -> [Char] -> Char
count open [] = 'A'
count open (l:ls)
    | elem l "([{<" = count (l:open) ls
count (o:os) (l:ls) = case [o, l] of
    "()" -> count os ls
    "[]" -> count os ls
    "{}" -> count os ls
    "<>" -> count os ls
    _ -> l

points :: [Char] -> Int
points [] = 0
points (x:xs) = case x of
    'A' -> points xs
    ')' -> 3 + points xs
    ']' -> 57 + points xs
    '}' -> 1197 + points xs
    '>' -> 25137 + points xs

solve :: [Char] -> Int
solve = points . map (count []) . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve
