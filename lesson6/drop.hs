mdrop :: Int -> [Int] -> [Int]
mdrop _ [] = []
mdrop 0 ns = ns
mdrop x (n:ns) =  drop (x-1) ns
