import Prelude hiding (all, any, takeWhile, dropWhile, map, filter)

all :: (Int -> Bool) -> [Int] -> Bool
all p = foldr (\x z -> p x && z) True

any :: (Int -> Bool) -> [Int] -> Bool
any p = foldr (\x z -> p x || z) False

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x:xs) | p x == True = x : takeWhile p xs
                   | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] = []
dropWhile p (x:xs) | p x == True = dropWhile p xs
                   | otherwise = x:xs

map :: (Int -> Int) -> [Int] -> [Int]
map f = foldr (\x xs -> f x : xs) []

filter :: (Int -> Bool) -> [Int] -> [Int]
filter f = foldr (\x xs -> if f x then x:xs else xs) []

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f v [] = v
myfoldr f v (x:xs) = f x (myfoldr f v xs)
