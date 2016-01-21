import Prelude hiding (length)

length :: [a] -> Int
length xs = foldr (\ _ n -> 1 + n) 0 xs
