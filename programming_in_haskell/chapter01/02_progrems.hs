-- 1
dobule' x = x * 2
doubledoble' x = dobule' $ dobule' x


-- 2
{--
  まず、
  sum の定義は

  sum [] = 0
  sum (x:xs) = x + sum xs

  である。sum の引数に[x]が渡された時
  sum x = x + []
  となるため明らかである。
--}


-- 3
product' [] = 1
product' (x:xs) = x * product' xs


-- 4
qsort' [] = []
qsort' (x:xs) = qsort' smaller ++ [x] ++ qsort' larger
                where
                  smaller = [a | a <- xs, a <= x]
                  larger  = [a | a <- xs, a >  x]

reqsort' [] = []
reqsort' (x:xs) = reqsort' larger ++ [x] ++ reqsort' smaller
                where
                  smaller = [a | a <- xs, a <= x]
                  larger  = [a | a <- xs, a >  x]

-- 5
-- ピボットと等しい場合smaller larger のどちらにも渡されない
qsort'' [] = []
qsort'' (x:xs) = qsort' smaller ++ [x] ++ qsort' larger
                where
                  smaller = [a | a <- xs, a < x]
                  larger  = [a | a <- xs, a >  x]
