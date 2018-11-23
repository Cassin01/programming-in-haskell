subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where
    yss = subs xs
