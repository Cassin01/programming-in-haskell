class Eq a where
    (==), (/=) :: a -> a -> Bool
    x /= y = (not (x == y))

instance Eq Bool where
  False == False = True
  True  == True  = True
  _     == _     = False

data Bool = False | True 
  deriving (Eq, Ord, Show, Read)

data Shape   = Circle Float | Rect Float Float
data Maybe a = Nothing | Just a
