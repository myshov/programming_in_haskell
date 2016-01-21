mapfilter :: (Int -> Int) -> (Int -> Bool) -> [Int] -> [Int]
mapfilter f p xs = map f (filter p xs)
