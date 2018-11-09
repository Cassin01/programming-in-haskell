double x = x + x

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum xs

qsort' :: Ord a => [a] -> [a]
qsort' [] = []
qsort' (x:xs) =
  qsort' smaller ++ [x] ++ qsort' larger
  where
    smaller = [a | a <- xs, a <= x]
    larger  = [b | b <- xs, b > x]
