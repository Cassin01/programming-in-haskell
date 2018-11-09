--import Data.List hiding (delete)
import System.IO
import Control.Applicative


---------------------------------
  -- perser
newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

---------------------------------

---------------------------------
  -- I can't fiure out
expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
           <|> do symbol "-"
                  e <- expr
                  return (t - e)
           <|> return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (f * t)
           <|> do symbol "/"
                  t <- term
                  return (f `div` t)
           <|> return f
---------------------------------

---------------------------------
beep :: IO ()
beep = putStr "\BEL"

cls :: IO ()
cls = putStr "\ESC[2J"
---------------------------------

goto :: (Int,Int) -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: (Int,Int) -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

---------------------------------

box :: [String]
box = ["+---------------+",
       "|               |",
       "+---------------+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

buttons :: [Char]
buttons = standard ++ extra
  where
    standard = "gcd=123+456-789 * 0()/"
    extra    = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = sequence_ [writeat (1, y) xs | (y, xs) <- zip [1..13] box]

display :: String -> IO ()
display xs = do
  writeat (3, 2) "             "
  writeat (3, 2) (reverse (take 13 (reverse xs)))


calc :: String -> IO ()
calc xs = do display xs 
             c <- getCh
             if elem c buttons then
                 process c xs
             else
                 do beep
                    calc xs

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

process :: Char -> String -> IO ()
process c xs
  | elem c "qQ\ESC"    = quit
  | elem c "dD\BS\DEL" = delete xs
  | elem c "=\n"       = eval xs
  | elem c "cC"        = clear
  | otherwise          = press c xs

quit :: IO ()
quit = goto (1, 14)

delete :: String -> IO ()
delete [] = calc []
delete xs = calc (init xs)

eval :: string -> io ()
eval xs = case parse expr xs of
  [(n, "")] -> calc (show n) 
  _         -> do beep
                  calc xs

clear :: IO ()
clear = calc ""

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do cls
         showbox
         clear
