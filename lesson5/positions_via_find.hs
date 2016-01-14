import Data.List
positions :: Eq a => a -> [a] -> [Int]
positions x xs = findIndices (==x) xs
