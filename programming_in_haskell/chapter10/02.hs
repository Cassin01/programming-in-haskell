-- リストから重複を取り除く
rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

--  全パターン列挙
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = do
  map (False:) bss ++ map (True:) bss
  where bss = bools (n - 1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where vs = rmdups (vars p)

{--
n == 0 :: 
  [[]]

n == 1 :: 
  map (False:) [[]] map (True:) [[]] -> [[False], [True]]

n == 2 :: 

  map (False:) [[False], [True]] -> [[False, False], [False, True]]
  map (True:)  [[False], [True]] -> [[True , False], [True, True]]

  [[False,False],[False,True]] ++ [[True,False],[True,True]]
    ->[[False,False],[False,True],[True,False],[True,True]]

n == 3
  map (False:) [[False,False],[False,True],[True,False],[True,True]]
    -> [[False, False, False],
        [False, False, True],
        [False, True,  False],
        [False, True,  True]]

  map (True:) [[False,False],[False,True],[True,False],[True,True]]
    -> [[True,False,False],
        [True,False,True],
        [True,True,False],
        [True,True,True]]

    [[False, False, False],
     [False, False, True],
     [False, True,  False],
     [False, True,  True]]

      ++ 

    [[True,False,False],
     [True,False,True],
     [True,True,False],
     [True,True,True]]

      --> 

[[True,False,False],[True,False,True],[True,True,False],[True,True,True],[
False,False,False],[False,False,True],[False,True,False],[False,True,True]
]
--}
