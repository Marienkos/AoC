dir :: String -> [String] -> [String]
dir s [] = [s]
dir s (x:xs)
    | x == "$ cd /" = dir "/" xs
    | and [length parole == 3, parole !! 2 /= ".."] = dir (s ++ (parole !! 2) ++ "/") xs
    | and [length parole == 3, parole !! 2 == ".."] = s : (dir s xs {- Non devo mettere tutto s, ma s meno la parte dopo l'ultimo / -})
    | otherwise = dir s xs 
        where parole = words x

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ dir " " (lines input)