import Control.Monad.State

main = do
  print (fib 5)

fib :: Int -> Int 
fib n = flip evalState (0,1) $ do
  forM [0..(n-1)] $ \_ -> do
    (a,b) <- get
    put (b, a + b)
  (a,b) <- get
  return a
