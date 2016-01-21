import Data.Char

type Bit = Int

unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

chop8 :: [Bit] -> [[Bit]]
chop8 [ ]= []
chop8 bits = take 8 bits: chop8 (drop 8 bits)

chop8' :: [Bit] -> [[Bit]]
chop8' = unfold null (take 8) (drop 8)

map' :: (Int -> Int) -> [Int] -> [Int]
map' f = unfold null (f.head) tail

iterate' :: (Int -> Int) -> Int -> [Int]
iterate' f = unfold (\x -> x /= x) id f
