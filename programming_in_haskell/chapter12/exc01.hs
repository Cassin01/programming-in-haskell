-- 1

{--
1 + (2 ∗ 3)
  (2 * 3) <- both

(1 + 2) ∗ (2 + 3)
  (1 + 2) -> innermost
  (2 + 3) -> outermost

fst (1 + 2, 2 + 3)
  (1 + 2)    -> innerost
  (2 + 3)
  fst (_, _) -> outermost

(\x -> 1 + x) (2 ∗ 3)
  (2 * 3)       -> innorremost
  (\x -> 1 + x) -> outermost
--}

-- 2
-- 評価を停止できること
-- また、
-- ２引数目が与えなれなくても実行ができること
--
-- Enable to stop evaluation,
-- or
-- Enable to  execute without second argument.

-- 3
-- mult = \x -> (\y -> x ∗ y)
  -- 0 mult 3 4
  -- 1 \x -> (\y -> x * y) 3 4
  -- 2 \x -> (x * 4)
  -- 3 (3 * 4)
  -- 4 12

-- 4
fibs :: [Integer]
fibs = 0 : 1 : [x + y | (x, y) <- zip fibs (tail fibs)]

-- [0:1] [1]
-- [0:1:2] [1:2]
-- [0:1:2:3] [1:2:3]

-- 5
fib :: Int -> Integer
fib n = fibs !! n
