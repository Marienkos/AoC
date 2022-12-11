split :: String -> [String]
split [] = []
split x = [(takeWhile (/= '/') x)] ++ (split $ drop 1 (dropWhile (/= '/') x))

unify :: [String] -> String
unify [] = []
unify (x:xs)
    | length (x:xs) > 1 = x ++ "/" ++ (unify xs)
    | otherwise = []

parse :: String -> [String] -> [String]
parse s [] = [s]
parse s (x:xs)
    | x == "$ cd /" = parse "/" xs
    | and [length parole == 3, parole !! 2 /= ".."] = s : (parse (s ++ (parole !! 2) ++ "/") xs)
    | and [length parole == 3, parole !! 2 == ".."] = s : (parse (unify $ split s) xs)
    | or [parole !! 1 == "ls", parole !! 0 == "dir"] = parse s xs
    | otherwise = (s ++ (parole !! 0)) : (parse s xs)
        where parole = words x

file :: [String] -> [String]
file [] = []
file (x:xs)
    | last x /= '/' = x : (file xs)
    | otherwise = file xs

folder :: [String] -> [String]
folder [] = []
folder (x:xs)
    | last x == '/' = x : (folder xs)
    | otherwise = folder xs

noduplicates :: [String] -> [String]
noduplicates [] = []
noduplicates (x:xs)
    | elem x xs = noduplicates xs
    | otherwise = x : (noduplicates xs)

substring :: String -> String -> Bool
substring _ [] = False
substring x (y:ys)
    | take (length x) (y:ys) == x = True
    | otherwise = substring x ys

sums :: String -> [String] -> Int
sums _ [] = 0
sums c (f:fs)
    | substring c f = (read (last $ split f) :: Int) + (sums c fs)
    | otherwise = sums c fs

solve :: [Char] -> Int
solve i = minimum [sums x files | x <- folders, 70000000-(sums "/" files)+(sums x files)>=30000000]
    where
        files = file $ parse " " (lines i)
        folders = noduplicates $ folder $ parse " " (lines i)

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ solve input
