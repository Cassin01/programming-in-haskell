import Data.List

add' :: Int -> Int -> Int
add' x y = x + y

add'' :: Int -> (Int -> Int)
add'' = \x -> (\y -> x + y)

twice :: (a -> a) -> a -> a
twice f x = f (f x)

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = [f x | x <- xs]

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p []                 = []
filter'' p (x:xs) | p x       = x : filter'' p xs
                  | otherwise = filter'' p xs

sumsqreven :: [Int] -> Int
sumsqreven ns = sum (map (^2) (filter even ns))

-- 7.3
{--
f [] = v
f (x:xs) = x ++ f xs
--}

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f v [] = v
myfoldr f v (x : xs) = f x (myfoldr f v xs)

mylength :: [a] -> Int
mylength [] = 0
mylength (_:xs) = 1 + mylength xs

msum = sum' 0
  where
    sum' v [] = v
    sum' v (x:xs) = sum' (v + x) xs

kutuku :: (b -> c) -> (a -> b) -> (a -> c)
f `kutuku` g = \x -> f (g x)



