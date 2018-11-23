data Tree = Leaf Int | Node Tree Tree

-- この木の葉の個数が、節の個数よりも、常に１多いこと -- (*)
-- を数学的帰納法を用いて示す

-- まず

countLeaf :: Num p => Tree -> p
countLeaf (Leaf x)     = 1
countLeaf (Node t0 t1) = countLeaf t0 + countLeaf t1

countNode :: Num p => Tree -> p
countNode (Leaf x)     = 0
countNode (Node t0 t1) = 1 + countNode t0 + countNode t1

testTree = Node (Node (Leaf 1)
                       (Leaf 1))
                 (Node (Node (Leaf 1)
                             (Leaf 1))
                       (Leaf 1))


{-- 関数を以上のように定義する。
  (*)が成り立つことを証明する。

  # 基底部
    t = Leaf x のとき
    countLeaf (Leaf x) - countNode (Leaf x) = 1 - 0
                                            = 1

   となり(*)は成り立つ。

 # 仮定
  t1, t2 ∈ Tree に対して
  countLeaf (t1) - countNode (t1) = 1
  countLeaf (t2) - countNode (t2) = 1
  であることを仮定する。


  # 帰納部
  t = Node t1 t2
  である時
  countLeaf t - countNode t =   (countLeaf t1 + countLeaf t2)
                              - (countNode t1 + countNode t2 + 1)
                            =   countLeaf (t1) - countNode (t1)
                              + countLeaf (t2) - countNode (t2)
                              - 1
                           = 1
 となりこの時も(*)は成立。

 以上より(*)は帰納法より証明された。
--}
