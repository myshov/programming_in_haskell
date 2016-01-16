mlength :: [Int] -> Int
mlength [] = 0
mlength (n:ns) = 1 + mlength ns
