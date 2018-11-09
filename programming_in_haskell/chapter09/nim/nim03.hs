-- file name : nim.hs

import System.IO
import Data.List
import Data.Char

-- for loop
import Control.Monad
import Data.Function


type Board = [Int]

main =
  let initialBoard = [1,2,3,4,5]
  in nim initialBoard

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
      xs <- readLn :: IO [Int]
      unless (all (<= 0) board) $ do
        showResult $
          giveNums $
            reverse $ 
              [if (head xs) == y then x - (tailVal xs) else x |
                (x, y) <- zip board [1..]]
        -- 繰り返し
        loop [if (head xs) == y then x - (tailVal xs) else x |
                (x, y) <- zip board [1..]]
  putStrLn "bye"

-- take value from a tail of list
tailVal ::[a] -> a
tailVal [x] = x
tailVal (x:xs) = tailVal xs

-- show
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
