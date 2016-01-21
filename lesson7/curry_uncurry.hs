simplesum :: (Int, Int) -> Int
simplesum (x, y) = x + y

curry' :: ((Int, Int) -> Int) -> (Int -> Int -> Int)
curry' f = \x -> \y -> f (x,y)

currysum :: Int -> Int -> Int
currysum = curry' simplesum

uncurry' :: (Int -> Int -> Int) -> ((Int, Int) -> Int)
uncurry' f = \(x, y) -> f x y

uncurrysum :: (Int, Int) -> Int
uncurrysum = uncurry' currysum
