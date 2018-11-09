-- file name : nim.hs

import System.IO
import Data.List
import Data.Char

-- for loop
import Control.Monad
import Data.Function


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

-- 4's fix
type Board = [Int]

main =
  let initialBoard = [1,2,3,4,5]
  in nim initialBoard

-- FIXME: ループにて同じ値を入れた時変わらない

nim :: Board -> IO ()
nim initialBoard = do
  putStrLn "start"
  showResult $
    giveNums $
      reverse [x | x <- initialBoard]
  -- ループ開始
  ($ initialBoard) . fix $
    \ loop board -> do
      putStr "Which take ? > "
      n <- readLn
      putStr "> "

      unless (n == 020) $ do
        showResult $
          giveNums $
            reverse $ 
              [if n == y then x - 1 else x | (x, y) <- zip board [1..]]
        loop [if n == y then x - 1 else x | (x, y) <- zip board [1..]]
        -- 繰り返し
  putStrLn "bye"


{--
-- thired fix
type Board = [Int]

main =
  let initialBoard = [1,2,3,4,5]
  in nim initialBoard

-- FIXME: ループにて同じ値を入れた時変わらない

nim :: Board -> IO ()
nim initialBoard = do
  putStrLn "start"
  showResult $
    giveNums $
      reverse [x | x <- initialBoard]
  -- ループ開始
  ($ initialBoard) . fix $
    \ loop board -> do
      putStr "> "
      n <- readLn
      unless (n == 020) $ do
        showResult $
          giveNums $
            reverse [if n == x then x - 1 else x | x <- board]
        loop [if n == x then x - 1 else x | x <- board]
        -- 繰り返し
  putStrLn "bye"
--}

-- second fix
{--
main :: IO ()
main = do
  putStrLn "start"
  ($ 3) . fix $ \ loop x -> do              -- ループ開始
    putStr "> "
    n <- readLn
    unless (n == 020) $ do -- "quit"じゃなければ次を実行
      showResult $
        giveNums $
          reverse [if n == x then x - 1 else x | x <- [1, 2, 3, 4, 5]]
      loop x                  -- 繰り返し
  putStrLn "bye"
--}

-- first fix
{--
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
--}

