safetail :: [a] -> [a]
safetail as | null as == True = []
            | otherwise = tail as


safetailV2 :: [a] -> [a]
safetailV2 as = if null as == True then [] else tail as


safetailV3 :: [a] -> [a]
safetailV3 [] = []
safetailV3 (a:as) = as
