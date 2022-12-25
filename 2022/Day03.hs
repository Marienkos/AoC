tripartize :: [String] -> [[String]]
tripartize [] = []
tripartize x = take 3 x : tripartize (drop 3 x)

threepartcommon :: [String] -> [Char]
threepartcommon l = [commmon x | x <- tripartize l]
    where commmon [x, y, z] = head [c | c <- ['A'..'z'], elem c x, elem c y, elem c z]

convert :: [Char] -> [Int]
convert l = [letter x | x <- l]
    where letter c = head [x | x <- [1..52], (['a'..'z']++['A'..'Z']) !! (x-1) == c]

solve :: [Char] -> Int
solve = sum . convert . threepartcommon . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve
