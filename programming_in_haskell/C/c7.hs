interleave x []       = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))
