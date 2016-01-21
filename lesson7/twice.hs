twice :: (a -> a) -> a -> a
twice f x = f (f x)
