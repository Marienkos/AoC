tpz :: [String] -> [[String]]
tpz [] = []
tpz x = (take 3 x) : (tpz $ drop 3 x)

cml :: [String] -> [Char]
cml l = [com x | x <- tpz l]
    where com [x, y, z] = head [c | c <- ['A'..'z'], elem c x, elem c y, elem c z]

conv :: [Char] -> [Int]
conv l = [letter x | x <- l]
    where letter c = head [x | x <- [1..52], (['a'..'z']++['A'..'Z']) !! (x-1) == c]

sol :: [Char] -> Int
sol = sum . conv . cml . lines

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ sol input