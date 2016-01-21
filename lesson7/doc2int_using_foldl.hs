dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10 * x +  y) 0
