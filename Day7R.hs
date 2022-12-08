{-

IDEA
Ogni file lo salvo come (profondità p, dimensione d)
Faccio la somma di tutto: se > 100.000, elimino i file con p=1 e ogni p=p-1
Continuo così ricorsivamente finché la somma totale non è < 100.000

-}


alb :: Int -> [String] -> [(Int, Int)] -- DA CORREGGERE
alb _ [] = []
alb n (x:xs)
    | and [length parole == 3, parole !! 2 /= ".."] = alb (n+1) xs
    | and [length parole == 3, parole !! 2 == ".."] = (n, 0) : (alb (n-1) xs)
    | or [parole !! 1 == "ls", parole !! 0 == "dir"] = alb n xs
    | otherwise = (n, read (parole !! 0) :: Int) : (alb n xs)
        where parole = words x

som :: [(Int, Int)] -> Int
som [] = 0
som (x:xs) = (snd x) + (som xs)

app :: [(Int, Int)] -> [(Int, Int)] -- DA CORREGGERE 
app x
    | som x > 5 = [(p-1, d) | (p, d) <- x, p > 1]
    | otherwise = x

-- Potrei creare una funzione che applica app a tutte le sottoliste con (p, 0)

smf :: [(Int, Int)] -> Int
smf [] = 0
smf (x:xs) = (snd x) * (fst x) + (smf xs)

sol :: [Char] -> Int
sol x = smf $ app $ alb 0 (lines x)

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ app [(1, 3), (1, 0), (1, 5)]

-- COSA DA ORA: [(2, 3), (2, 3), (1, 1)]
-- COSA DOVREBBE DARE: [(3, 3), (3, 0), (3, 3), (2, 1)]