data Point = Point {x :: Int, y :: Int}
    deriving Show

decode :: String -> Point
decode x = Point (read $ takeWhile (/= ',') x) (read $ tail $ dropWhile (/= ',') x)

area :: Point -> Point -> Int
area (Point a b) (Point c d) = abs ((c-a+1)*(d-b+1))

largest :: [Point] -> Int
largest l = maximum [area a b | a <- l, b <- l]

minX = minimum . map x
minY = minimum . map y
maxX = maximum . map x
maxY = maximum . map y

rectangle :: [Point] -> [[Point]]
rectangle l = [[Point x y | x <- [minX l..maxX l]] | y <- [minY l.. maxY l]]

perimeter :: [Point] -> [Point]
perimeter [a] = []
perimeter (Point x1 y1 : Point x2 y2 : ps)
    | x1 == x2 && y1 < y2 = [Point x1 y | y <- [y1..y2-1]] ++ perimeter (Point x2 y2 : ps)
    | x1 == x2 && y1 > y2 = [Point x1 y | y <- reverse [y2..y1-1]] ++ perimeter (Point x2 y2 : ps)
    | y1 == y2 && x1 < x2 = [Point x y1 | x <- [x1..x2-1]] ++ perimeter (Point x2 y2 : ps)
    | y1 == y2 && x1 > x2 = [Point x y1 | x <- reverse [x2..x1-1]] ++ perimeter (Point x2 y2 : ps)

solve x = p
    where
        base = map decode $ lines x
        p = init $ perimeter (base ++ [head base])

main :: IO ()
main = readFile "input.txt" >>= print . solve