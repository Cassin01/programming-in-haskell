-- note


add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add n m)

replicate :: Int -> a -> [a]
replicate 0 _ = [] replicate n c = c : replicate (n - 1) c

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

-- 1 reverse' xs ys = reverse xs ++ ys を満たすものを模索
-- 2 reverse  xs    = reverse' xs [] reverse :: [a] -> [a]
reverse xs = reverse' xs []

reverse' :: [a] -> [a] -> [a]
reverse' [] ys = ys
reverse' (x:xs) ys = reverse' xs (x:ys)

-- reverse use foldl
reverse = foldl (\xs x -> x : xs) []

-- 二分木
data Tree = Leaf Int | Node Tree Tree
flatten :: Tree -> [Int]
flatten (Leaf n)   = [n]
flatten (Node l r) = flatten l ++ flatten r

-- 効率をあげる
flatten' :: Tree -> [Int] -> [Int]
flatten' (Leaf n)   ns = n : ns
flatten' (Node l r) ns = flatten' l (flatten' r ns)

flatten :: Tree -> [Int]
flatten t = flatten' t []


-- 13.7 プログラムの論証
data Expr = Val Int | Add Expr Expr
eval :: Expr -> Int
eval (Val n)   = n
eval (Add x y) = eval x + eval y

-- を評価
type Stack = [Int]
type Code  = [Op]
data Op    = PUSH Int | ADD

-- スタックの
--   一番上 y
--   その次 x
exec :: Code -> Stack -> Stack
exec [] s                  = s
exec (PUSH n : c) s        = exec c (n : s)
exec (ADD : c) (m : n : s) = exec c (n + m : s)

comp :: Expr -> Code
comp (Val n) = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

{-- 証明

# (execの実行結果が評価結果のリストになることの証明)
  exec (comp e) [] = [eval e] を証明する

## 仮定
  exec (comp x) s = eval x : s
  exec (comp y) s = eval y : s
  を仮定する


# (分配法則の証明)
  exec (c ++ d) s == exec d (exec c s) を証明する

## 基底部
  c = [] のとき

## 仮定
  c = x のとき
  exec (x ++ d) s = exec d (exec x s)
  が成り立つと仮定する

## 帰納部
  c = PUSH n : x のとき (再帰部1)
  左辺 = exec((PUSH n : x) ++ d) s
       = exec(PUSH n : (x ++ d)) s

                      { 仮定x部  }
                          ↓
  右辺 = exec d (exec (PUSH n : x) s)
       = exec ((PUSH n : x) ++ d) s
       = exec (PUSH n : (x ++ d)) s

  c = ADD : x のとき (再帰部2)
  左辺 = exec((ADD : x) ++ d) s
       = exec(ADD : (x ++ d)) s

                      { 仮定x部  }
                          ↓
  右辺 = exec d (exec (ADD : x) s)
       = exec ((ADD : x) ++ d) s
       = exec (ADD : (x ++ d)) s

  * 再帰部2の証明ではADD命令が実行される際にはスタックに少なくとも二つの
    整数があることを保証する

  * コンパイラのスタックアンダーフローを防ぐ
--}

{-- スタックアンダーフローを防ぐ方法 → 結合演算子の除去
   # comp' e c = comp e ++ c の模索
--}

comp :: Expr -> Code
comp (Val n) = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

-- から

comp' :: Expr -> Code -> Code
comp' (Val n)   c = PUSH n : c
comp' (Add x y) c = comp' x (comp' y (ADD : c))

-- へ

comp e = comp' e []



{-- 新 comp' の正しさの証明
   exec (comp' e c) s = exec c (eval e : s)

  を証明する
  (s = [], c = [], のとき exec (comp e) [] = [eval e] になることに注意)

  # 証明

  ## 基底部
  e = Val n のとき
  左辺 = exec (comp' (Val n) c) s
  右辺 = exec c (eval (Val n) : s)
       = exec (comp' (Val n) c) s

  ## 仮定部
  e = x と e = y のとき

    exec (comp' x c) s = exec c (eval x : s)
    exec (comp' y c) s = exec c (eval y : s)

  を仮定する

  ## 帰納部
  e = Add x y のとき
    左辺 = exec (comp' (Add x y) c) s
         = exec (comp' x (comp' y (ADD : c))) s
         = exec (comp' y (ADD : c))) (exec x : s)
         = exec (ADD : c) (eval y : eval x : s)
         = exec c ((eval y + eval y) : s)

    右辺 = exec c (eval (Add x y) : s)
         = exec c ((eval x + eval y) : s)

                                         [証明終了]
--}

exec :: Code -> Stack -> Stack
exec [] s                  = s
exec (PUSH n : c) s        = exec c (n : s)
exec (ADD : c) (m : n : s) = exec c (n + m : s)

eval :: Expr -> Int
eval (Val n)   = n
eval (Add x y) = eval x + eval y
