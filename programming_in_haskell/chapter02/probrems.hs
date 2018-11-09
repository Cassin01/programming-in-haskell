-- 1
{-
2 ^ 3 * 4
2 * 3 + 4 * 5
2 + 3 * 4 ^ 5

(2 ^ 3) * 4
(2 * 3) + (4 * 5)
2 + ((3 * 4) ^ 5)
-}

-- 3
{-
   N = a `div` length xs
      where
        a  = 10
        xs = [1,2,3,4,5]
-}
n = a `div` (length xs)
  where
    a  = 10
    xs = [1,2,3,4,5]

-- 4
last' xs = head $ drop ((length xs) - 1) xs

last'' [x]    = x
last'' (x:xs) = last'' xs

-- 5
init' xs = reverse $ tail $ reverse xs

init'' xs = take ((length xs) - 1) xs
