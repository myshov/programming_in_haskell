positions :: Eq a => a -> [a] -> [Int]
positions x xs = 
    [i | (x', i) <- zip xs [0..n], x == x']
    where n = length xs - 1
