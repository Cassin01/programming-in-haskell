split :: [a] -> [([a], [a])]
split []  = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]

-- split [1,2,3,4]
-- ([1], [1,2,3,4]) : R0

-- split [2,3,4]
-- ([2], [3,4]) : R1

-- split [3,4]
-- ([3], [4]) : R2

-- split [4]
-- []

exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls, rs) <- split ns,
                l <- exprs ls,
                r <- exprs rs,
                e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mud, Div]

results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns,
              lx <- results ls,
              ry <- results rs,
              res <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = [(App o l r, apply o x y) | o <- ops,
                         valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e, m) <- results ns', m == n]

{--
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0
--}

  {--
valid Add x y = x <= y
valid Sub x y = x >  y
valid Mul x y = x /= 1 and y /= 1 nad x <= y
valid Div x y = y /= 1 and x `mod` y == 0
  --}
