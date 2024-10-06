decode :: [Char] -> (Char, Int)
decode x = (head x, read [last x])

follow :: (Int, Int) -> [(Char, Int)] -> Int
follow (p, d) [] = p*d
follow (p, d) (('f', n):xs) = follow (p+n, d) xs
follow (p, d) (('d', n):xs) = follow (p, d+n) xs
follow (p, d) (('u', n):xs) = follow (p, d-n) xs

main :: IO ()
main = readFile "input.txt" >>= print . follow (0, 0) . map decode . lines