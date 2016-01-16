import Prelude hiding (sum, take, last)

sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

take :: Num a => Int -> [a] -> [a]
take _ [] = []
take 0 _ = []
take n (x:xs) = x : take (n-1) xs

last :: Num a => [a] -> a
last [x] = x
last (_:xs) = last xs
