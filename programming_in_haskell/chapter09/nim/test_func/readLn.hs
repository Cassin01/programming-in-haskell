{--
main = do
    n <- readLn -- input: 55
    putStrLn . show $ n * n -- output: 3025
--}
main = do
  xs <- readLn :: IO [Int] -- input
  putStrLn . show $ xs -- output

