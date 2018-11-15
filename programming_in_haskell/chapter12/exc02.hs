-- 6
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving(Show)

take :: Int -> Tree a -> Tree a
take 0 t      = Node
take n t = Node t' a t'
  where t'        = takeTree (n - 1) t a

repeat' :: a -> Tree a
repeat' x = Node t x t
                where t = repeat' x

take' :: Int -> Tree a -> Tree a
take' 0 _            = Leaf
take' x Leaf         = Leaf
take' x (Node l y r) = Node (take' (x - 1) l) y (take' (x - 1) r)

replicate' :: Int -> a -> Tree a
replicate' n = take' n . repeat
