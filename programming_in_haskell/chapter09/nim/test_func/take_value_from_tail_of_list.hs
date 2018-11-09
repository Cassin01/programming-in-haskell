main = do
  print . show $ tail_value [1,3,3,4]

tail_value :: Num a => [a] -> a
tail_value [x] = x
tail_value (x:xs) = tail_value xs

