import Prelude hiding (and, concat, replicate, (!!), elem)

and :: [Bool] -> Bool
and [] = True
and (n:ns) = n && and ns

concat :: [[a]] -> [a]
concat [] = []
concat (n:ns) = n ++ (concat ns)

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x

(!!) :: [a] -> Int -> a
(n:ns) !! 0 = n
(n:ns) !! i = ns !! (i-1)

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem i (n:ns) | n == i = True
              | otherwise =  elem i ns
