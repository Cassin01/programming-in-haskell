import Data.List

-- 1
halve :: [a] -> ([a], [a])
halve xs = ( take ((length xs) `div` 2) xs, drop ((length xs) `div` 2) xs )

-- 2
-- a
safetail :: [a] -> [a]
safetail xs =
  if length xs == 0 then []
  else tail xs

-- b
safetail' :: [a] -> [a]
safetail' xs
  | length xs == 0 = []
  | otherwise   = tail xs

-- c
safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' xs = tail xs

-- 3
oa' :: Bool -> Bool -> Bool
True  `oa'` True  = True
True  `oa'` False = True
False `oa'` True  = True
False `oa'` False = False

oa'' :: Bool -> Bool -> Bool
False `oa''` False = False
_     `oa''` _     = True

oa''' :: Bool -> Bool -> Bool
False `oa'''` b = b
True  `oa'''` _ = True

oa'''' :: Bool -> Bool -> Bool
b  `oa''''` False = b
_  `oa''''` True  = True

cdand :: Bool -> Bool -> Bool
cdand a b =
  if a == True && b == True then b
  else False

-- 6
curry_mult = \x -> (\y -> (\z -> x * y * z))
