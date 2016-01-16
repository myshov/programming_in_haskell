-- function for edge case when there is odd number of elemnets in the list
swaplast :: Ord a => [a] -> [a]
swaplast xs = if y > z then xsBegin ++ [z] ++ [y] else xsBegin ++ [y] ++ [z]
               where y = xs !! ((length xs) - 2)
                     z = xs !! ((length xs) - 1)
                     xsBegin = take ((length xs) - 2) xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y = [x,y] ++ merge xs ys
                    | otherwise = [y,x] ++ merge xs ys


halve :: Ord a => [a] -> ([a], [a])
halve [] = ([], [])
halve xs = (take halfIdx xs, drop halfIdx xs)
           where halfIdx = length xs `quot` 2

msortPriv :: Ord a => [a] -> [a]
msortPriv [] = []
msortPriv [x] = [x]
msortPriv xs = merge (msortPriv lxs) (msortPriv rxs)
               where (lxs, rxs) = halve xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = swaplast (msortPriv xs)
