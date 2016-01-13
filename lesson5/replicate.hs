replicate_ :: Int -> a -> [a]
replicate_ n x = [x | _ <- [1..n]]
