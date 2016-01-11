add :: (Int, Int) -> Int
add (x, y) = x + y

zeroto :: Int -> [Int]
zeroto n = [0..n]

add' :: Int -> Int -> Int
add' x y = x + y

mult :: Int -> Int -> Int -> Int
mult x y z = x * y * z

second :: [a] -> a
second xs = head (tail xs)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => Int -> Int
double x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x  = f (f x)

triple :: (a -> a) -> a -> a
triple f x = f (f (f x))
