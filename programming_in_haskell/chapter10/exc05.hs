-- 恒 真 式 か 検 査 する 関 数 を 拡 張 して、 
-- 命 題 に 論 理 和 （∨） と 同 値 （⇔） が 使 えるようにせよ。

data Prop = Or Prop Prop | Equiv Prop Prop

eval s (Or p q)    = eval s p || eval s q
eval s (Equiv p q) = eval s p == eval s q

vars (Or p q)      = vars p ++ vars q
vars (Equiv p q)   = vars p ++ vars q
