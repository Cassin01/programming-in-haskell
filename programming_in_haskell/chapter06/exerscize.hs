import Data.List
import Debug.Trace
-- 1
up' :: Int -> Int -> Int
up' x 0 = 1
up' x n = x * (up' x (n - 1))

-- 3
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x : xs) = x ++ (concat' xs)

replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x = [x] ++ replicate' (n - 1) x

eqs' :: [a] -> Int -> a
eqs' (x:xs) 0  = x
eqs' (x:xs) n = eqs' xs (n - 1)

elem' :: Eq a => a -> [a] -> Bool
elem' y []     = False
elem' y (x:xs) = if x == y then True else elem' y (xs)

-- 4
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) (ys)

-- 5
--main = print (msort [2,3,1,4,7,2,9,1])

msort :: Ord a => [a] -> [a]
msort []    = []
msort (x:xs)    = merge [x] (msort xs)

-- 6
-- main = print (sum' [1,2,3,3])

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum xs

-- 7
-- main = print (take' 2 [1,2,3,3])

take' :: Int -> [a] -> [a]
take' 0 xs = []
take' _ [] = []
take' n (x:xs) = x : (take' (n - 1) xs)

-- 8
-- main = print (last' [1,2,3,7])
last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs
