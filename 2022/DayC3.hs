tripartize :: [String] -> [[String]]
tripartize [] = []
tripartize x = (take 3 x) : (tripartize $ drop 3 x)

cml :: [String] -> [Char]
cml l = [com x | x <- tripartize l]
    where com [x, y, z] = head [c | c <- ['A'..'z'], elem c x, elem c y, elem c z]

conv :: [Char] -> [Int]
conv l = [letter x | x <- l]
    where letter c = head [x | x <- [1..52], (['a'..'z']++['A'..'Z']) !! (x-1) == c]

solve :: [Char] -> Int
solve = sum . conv . cml . lines

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ solve input
