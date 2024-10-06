decode :: [Char] -> (Char, Int)
decode x = (head x, read [last x])

follow :: (Int, Int, Int) -> [(Char, Int)] -> Int
follow (p, d, a) [] = p*d
follow (p, d, a) (('f', n):xs) = follow (p+n, d+(a*n), a) xs
follow (p, d, a) (('d', n):xs) = follow (p, d, a+n) xs
follow (p, d, a) (('u', n):xs) = follow (p, d, a-n) xs

main :: IO ()
main = readFile "input.txt" >>= print . follow (0, 0, 0) . map decode . lines