type IO a = World -> (a, World)

-- getChar
-- putChar

return :: a -> IO a
return c = \ world -> (v, world)

(>>=) :: IO a -> (a -> IO b) -> IO b
f >>= g = \ world -> case f world of
                      (v, world') -> g v world'

echo :: IO ()
echo = do c <- getChar
          putChar '\n'
          putChar c
          putChar '\n'

getLine :: IO String
getLine = do 
  x <- getChar
  if x == '\n' then
    return []
  else
    do 
      xs <- getLine
      return (x:xs)

  {--
putStr :: String -> IO ()
putStr []       = return ()
putStr (x : xs) = do 
  putChar x
  putStr xs
  --}

putStr xs = seqn [putChar x | x <- xs]

putStrLn :: String -> IO ()
putStrLn = do
  putStr xs
  putChar '\n'

strlen :: IO ()
strlen = do
  putStr "Enter a string: "
  xs <- getLine
  putStr "The string has "
  putStr (show (length xs))
  putStrLn " characters"

beep :: IO ()
beep = putStr "\BEL"

cls :: IO ()
cls = putSTr "\ESC[2j"

type Pos = (Int, Int)

goto :: Post -> IO ()
goto (x, y) = 
  putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do 
  goto p
  putStr xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a : as) = do
  a
  seqn as
