-- file name : nim.hs
import System.IO
import Data.List
import Data.Char


-- main = showResult $ giveNums $ reverse [1, 2, 3, 4, 5]

showResult a = do
  if length a == 0
    then return 0
    else do
      putStrLn $ (show (length a)) ++ " : " ++ (head a)
      showResult (tail a)

makeAst :: Int -> String
makeAst n = ['*' | x <- [1..n]]

giveNums xs = map showAst xs

showAst n = makeAst n

main = do
    putStr "> "
    n <- readLn
    if  n == 010  -- exit
      then return ()
      else do
        showResult $
          giveNums $
            reverse [if n == x then x - 1 else x | x <- [1, 2, 3, 4, 5]]
        main
