data Nat = Zero | Succ Nat
  deriving (Eq, Ord, Show)

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat n)

add' Zero n = n
add' (Succ m) n = Succ (add' m n)

mult :: Nat -> Nat -> Nat
mult m Zero = m
mult m n = int2nat $ add' m n'
  where
    nextn = int2nat $ (nat2int n) - 1
    n'    = mult m nextn
