-- file name : test_loop.hs

main = do
    putStr "> "
    str <- getLine
    if str == "exit"
      then return ()
      else do
        putStrLn str
        main