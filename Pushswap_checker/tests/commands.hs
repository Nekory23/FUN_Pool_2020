module Commands where

-- sa - sb - sc --
swap :: [Int] -> [Int]
swap [] = []
swap list
    | length list < 2 = list
    | otherwise = head (tail list) : head list : tail (tail list)

-- pa - pb --
pa :: ([Int], [Int]) -> ([Int], [Int])
pa (l_a, []) = (l_a, [])
pa (l_a, (b:bs)) = (b:l_a, bs)

pb :: ([Int], [Int]) -> ([Int], [Int])
pb ([], l_b) = ([], l_b)
pb ((a:as), l_b) = (as, a:l_b)

-- ra rb rr --
r_ab :: [Int] -> [Int]
r_ab [] = []
r_ab list@(start:rest)
    | length list < 2 = list
    | otherwise = rest ++ [start]

-- rra rrb rrr --
rr_ab :: [Int] -> [Int]
rr_ab [] = []
rr_ab list
    | length list < 2 = list
    | otherwise = [last list] ++ init list