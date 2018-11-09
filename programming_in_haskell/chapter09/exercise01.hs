-- 1

readLine :: IO String
readLine = do 
  x <- getChar
  if x == '\n' then
    return []
  else
    if x == '\DEL' then
      return (drop 1 xs)
    else
      if x == '\ESC[1D' then
        return (xs)
      else
        do 
          xs <- getLine
          return (x:xs)

