hashToInt :: Int -> String -> [Int]
hashToInt _ [] = []
hashToInt n (x:xs) = case x of
    '.' -> hashToInt (n+1) xs
    '#' -> n : hashToInt (n+1) xs
    _   -> hashToInt n xs

split :: String -> [Int]
split [] = []
split x = read (takeWhile (/= ',') x) : split (drop 1 (dropWhile (/= ',') x))

numToInt :: String -> [Int]
numToInt pls = split $ tail $ init pls

decode :: String -> ([Int], [[Int]])
decode x = (config, map numToInt $ init $ tail $ words x)
    where config = hashToInt 0 $ head $ words x

add :: [Int] -> [Int] -> [Int]
add a [] = a
add a b = [x | x <- [0..m], elem x a /= elem x b]
    where m = max (maximum a) (maximum b)

combCheck :: Int -> [[Int]] -> [[Int]] -> [Int] -> [[Int]] -> [[Int]] -> [[Int]] -> Int
combCheck n arc_og arc_old config _ [] new =
    combCheck (n+1) arc_og new config arc_og new [] -- Cambio di livello

combCheck n arc_og arc_old config [] (_:olds) new =
    combCheck n arc_og arc_old config arc_og olds new -- Prima passata

combCheck n arc_og arc_old config og@(og1:ogs) old@(old1:olds) new
    | current == config = n -- Caso in cui la combinazione attuale è la configurazione
    | elem current new = combCheck n arc_og arc_old config ogs old new -- C'è di già (no doppioni)
    | otherwise = combCheck n arc_og arc_old config ogs old (current:new) -- Else
        where current = add og1 old1

solveOnce :: ([Int], [[Int]]) -> Int
solveOnce (a, b) = combCheck 1 b [[]] a b [[]] []

solve :: String -> Int
solve = sum . map (solveOnce . decode) . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve