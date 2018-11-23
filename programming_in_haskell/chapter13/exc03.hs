-- 10

data Expr = Val Int | Add Expr Expr deriving (Show)

--eval:: Expr -> Int
--eval (Val n) = n
--eval (Add x y) = eval x + eval y

type Stack = [Int]
type Code = [Op]
data Op = PUSH Int | ADD deriving (Show)



comp:: Expr -> Code
comp (Val n) = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

comp' e c = comp e ++ c

{--
  # comp 基底部において
  comp' (Val n) c
  comp n ++ c
  [PUSH n] ++ c
  PUSH n : c

  # comp 再帰部において
  comp' Add (x y) c
  comp x ++ comp y ++ [ADD] ++ c
  comp x ++ comp y ++ (ADD : c)
  comp x ++ comp y ++ (ADD : c)
  comp x ++ comp' y (ADD : c)
  comp' x (comp' y (ADD : c)

--}
-- 以上より

comp' (Val n) c = PUSH n : c
comp' (Add x y) = comp' y (ADD : c)
