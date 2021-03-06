import Data.Char

concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

firsts' :: [(a, b)] -> [a]
firsts' ps = [x | (x, _) <- ps]

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

factors' :: Int -> [Int]
factors' n = [x | x <- [1..n], n `mod` x == 0]

prime' :: Int -> Bool
prime' n = factors' n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime' x]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)
-- 1 2 3 4
-- 2 3 4

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..n], x==x']
  where n = length xs - 1

lowers :: String -> Int
lowers xs = length [x | x <- xs, isLower x]

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']
