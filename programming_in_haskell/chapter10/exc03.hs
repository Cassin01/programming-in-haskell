data Tree = Leaf Int | Node Tree Tree

t :: Tree
t = Node (Node (Leaf 1) (Leaf 4)) (Node (Leaf 6) (Leaf 9))


t0 :: Tree
t0 = Node (Node (Leaf 1) (Leaf 4)) (Leaf 1)

t1 :: Tree
t1 = (
    Node (
      Node (
        Node (
          Leaf 5
        )
        (
          Leaf 4
        )
      )
      (
        Node (
          Leaf 6
        )
        (
          Leaf 4
        )
      )
    )
    (
      Node (
        Node (
          Leaf 6
        )
        (
          Leaf 4
        )
      )
      (
        Node (
          Leaf 6
        )
        (
          Leaf 4
        )
      )
    )
  )


balanced :: Tree -> Bool
balanced (Leaf n) = True
balanced (Node l r) = if abs (contLeaf l - contLeaf r) <= 1 then balanced l && balanced r else False

contLeaf :: Tree -> Int
contLeaf (Leaf n) = 1
contLeaf (Node l r) = contLeaf l + contLeaf r
