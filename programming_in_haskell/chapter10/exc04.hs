data Tree = Leaf Int | Node Tree Tree
  deriving (Show)

list :: [Int]
list = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

splitAtMostdiff1 :: [Int] -> ([Int], [Int])
splitAtMostdiff1 xs = splitAt (length xs `div` 2) xs

balance :: [Int] -> Tree
balance [x] = Leaf x
balance xs  = Node (balance l) (balance r)
  where (l, r) = splitAtMostdiff1 xs
