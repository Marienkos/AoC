coda :: [a] -> [a]
coda [] = []
coda (x:xs) = xs

spt :: String -> [String]
spt [] = []
spt x = [(takeWhile (/= '/') x)] ++ (spt $ coda (dropWhile (/= '/') x))

cat :: [String] -> String
cat [] = []
cat (x:xs)
    | length (x:xs) > 1 = x ++ "/" ++ (cat xs)
    | otherwise = []

prs :: String -> [String] -> [String]
prs s [] = [s]
prs s (x:xs)
    | x == "$ cd /" = prs "/" xs
    | and [length parole == 3, parole !! 2 /= ".."] = s : (prs (s ++ (parole !! 2) ++ "/") xs)
    | and [length parole == 3, parole !! 2 == ".."] = s : (prs (cat $ spt s) xs)
    | or [parole !! 1 == "ls", parole !! 0 == "dir"] = prs s xs
    | otherwise = (s ++ (parole !! 0)) : (prs s xs)
        where parole = words x

fil :: [String] -> [String]
fil [] = []
fil (x:xs)
    | last x /= '/' = x : (fil xs)
    | otherwise = fil xs

fld :: [String] -> [String]
fld [] = []
fld (x:xs)
    | last x == '/' = x : (fld xs)
    | otherwise = fld xs

dup :: [String] -> [String]
dup [] = []
dup (x:xs)
    | elem x xs = dup xs
    | otherwise = x : (dup xs)

infi :: String -> String -> Bool
infi _ [] = False
infi x (y:ys)
    | take (length x) (y:ys) == x = True
    | otherwise = infi x ys

som :: String -> [String] -> Int
som _ [] = 0
som c (f:fs)
    | infi c f = (read (last $ spt f) :: Int) + (som c fs)
    | otherwise = som c fs

sol :: [Char] -> Int
sol i = minimum [som x files | x <- folders, 70000000-(som "/" files)+(som x files)>=30000000]
    where
        files = fil $ prs " " (lines i)
        folders = dup $ fld $ prs " " (lines i)

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ sol input
