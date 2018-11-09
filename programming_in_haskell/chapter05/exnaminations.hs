import Data.List
-- 1
ans = [x ^ 2 | x <- [1..100]]

-- 2
repilcate :: Int -> a -> [a]
repilcate n a = [a | x <- [1..n]]

-- 3
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x ^ 2 + y ^ 2 == z ^ 2]

-- Ver not Duplication
pyths' :: Int -> [(Int, Int, Int)]
pyths' n = [(x, y, z) | x <- [1..n], y <- [x..n], z <- [y..n], x ^ 2 + y ^ 2 == z ^ 2]

-- 4
-- return all divisor
factors_except_me :: Int -> [Int]
factors_except_me n = [x | x <- [1..n], n `mod` x == 0 && x /= n]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors_except_me x) == x]

-- 5
example5 = [(x, y) | x <- [1,2,3], y <- [4,5,6]]

ans5 = concat [[(x, y) | x <- [1..3]] | y <- [4..6]]

-- 6
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']
--  where n = length xs - 1

positions''' :: Eq a => a -> [a] -> [Int]
positions''' x xs = [i | (x', i) <- zip xs [0..], (find (== x) [x']) == Just x']

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]
