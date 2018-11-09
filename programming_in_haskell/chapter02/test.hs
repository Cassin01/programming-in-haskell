double x = x + x

quadrupe x = double (double x)

factorial n = product [1..n]
average ns = sum ns `div` length ns

{-
a = b + c
  where
    b = 1
    c = 2
d = a * 2
-}

a = b + c
  where
    {b = 1;
     c = 2}
d = a * 2
