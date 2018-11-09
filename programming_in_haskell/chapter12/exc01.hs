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


-- 評価を停止できること
