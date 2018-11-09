{--
type String = [Char]

type Board = [Pos]
type Pos   = (Int, Int)

type Parser a = String -> [(a, String)]
type IO a     = World  -> (a, World)

type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

data Bool = False | True
--}
type Board = [Pos]
type Pos = (Int, Int)

data Move = Left | Right | Up | Down

move :: Move -> Pos -> Pos
move Left  (x, y) = (x - 1, y)
move Right (x, y) = (x + 1, y)
move Up    (x, y) = (x, y - 1)
move Down  (x, y) = (x, y + 1)

moves :: [Move] -> Move
moves [] p = p   
moves (m:ms) p = moves ms (move m p)

flip :: Move -> Move
flip Left  = Right
flip Right = Left
flip Up    = Down
flip Down  = Up

data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rect r)   = x * y
