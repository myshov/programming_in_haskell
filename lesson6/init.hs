minit :: [Int] -> [Int]
minit [n] = []
minit (n:ns) = n : (minit ns)
