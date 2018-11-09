data Expr = Val Int | Add Expr Expr | Mult Expr Expr
type Cont  = [Op]
data Op    = EVALA Expr | ADD Int | EVALM Expr | MUL Int

eval :: Expr -> Cont -> Int
eval (Val n)    ops = exec ops n
eval (Add x y)  ops = eval x (EVALA y : ops)
eval (Mult x y) ops = eval x (EVALM y : ops)

exec :: Cont -> Int -> Int
exec [] n              = n
exec (EVALA y : ops) n = eval y (ADD n : ops)
exec (ADD n : ops) m   = exec ops (n + m)
exec (EVALM y : ops) n = eval y (MUL n : ops)
exec (MUL n : ops) m   = exec ops (n * m)

value :: Expr -> Int
value e = eval e []
