pairs :: String -> Bool
pairs [_] = False
pairs (a:b:c) = a == b || pairs (b:c)

banned :: String -> Bool
banned [_] = True
banned (a:b:c) = notElem [a, b] ["ab", "cd", "pq", "xy"] && banned (b:c)

nice :: String -> Bool
nice x = length (filter (flip elem "aeiou") x) > 2 && pairs x && banned x

solve :: String -> Int
solve = length . filter nice . lines

main :: IO ()
main = readFile "input.txt" >>= print . solve
