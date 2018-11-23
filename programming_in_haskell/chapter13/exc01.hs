-- 1
-- drop 関数

add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add n m)


{-- 2 add n (Succ m) = Succ (add n m) を数学帰納法を用いて証明する

  # 基底部
    n = Zero のとき
      {左辺} = add Zero (Succ m)
             = Succ m
      {右辺} = Succ (add Zero m)
             = Succ m

  # 仮定
    n = n' のとき
      add n' (Succ m) = Succ (add n' m)
    が成り立つと仮定する

  # 帰納部
    n = Succ n' のとき
      {左辺} = add (Succ n') (Succ m)
             = Succ (add n' (Succ m))
             = Succ (Succ (add n' m))
      {右辺} = Succ (add (Succ n') m)
             = Succ (Succ (add n' m))

  よって示された。
--}


{-- 3
  add n m = add m n を

  add n Zero = n                  -- ① 
  add n (Succ m) = Succ (add n m) -- ② 
  数学的帰納法

  を用いて証明する

  # 基底部
    n = Zero のとき
      {左辺} = add Zero m
             = m
      {右辺} = add m Zero
             = Zero               ∵ ① 

  # 仮定
    n = n' のとき
        add n' m = add m n'
    が成り立つと仮定する

  # 帰納部
    n = Succ n' のとき
      {左辺} = add (Succ n') m
             = Succ (add n' m)
      {右辺} = add m (Succ n')
             = Succ (add m n')    ∵ ② 
             = Succ (add n' m)    ∵ 仮定

  よって示された。
--}

-- 4
replicate:: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x: replicate (n - 1) x

all p []     = True
all p (x:xs) = p x ^ all p xs

{--  の仮定の元
   all (== x) (replicate n x) が満たされることを示す

  # 基底部
    n = 0 のとき
      = all (== x) (replicate 0 x)
      = all (== x) []
      = True

  # 仮定
    n = k のとき
      all (== x) (replicate k x) が満たされると仮定する

  # 再帰部
    n = k + 1 のとき

    = all (== x) (replicate (k + 1) x)
    = all (== x) (x : (replicate k x))
    = (== x) x ^ all (== x) (replicate k x)
    = True ^ True
    = True
--}

{-- 5
  [] ++ ys = ys                        -- ① 
  (x:xs) ++ ys = x : (xs ++ ys)        -- ② 

  の元で

  xs ++ [] = xs                        -- ① '
  xs ++ (ys ++ zs) = (xs ++ ys) ++ zs  -- ② '

  が成り立つことを示す。

  まず ① ' を示す。

  # 基底部
    xs = [] のとき
      {左辺} = [] ++ []
      {右辺} = []

  # 仮定
    xs = xs' のとき
      xs' ++ [] = xs'
    が成り立つことを仮定する

  # 再帰部
    xs = x:xs'  のとき
      (x:xs') ++ [] = x : (xs ++ [])
                    = x:xs

  次に ② ' を示す。

  # 基底部
    xs = [] のとき
      {左辺} = xs ++ (ys ++ zs)
             = [] ++ (ys ++ zs)
             = ys ++ zs                ∵ ① 

      {右辺} = ([] ++ ys) ++ zs
             = ys ++ zs                ∵ ① 

  # 仮定
    xs = xs' のとき
      xs' ++ (ys ++ zs) = (xs' ++ ys) ++ zs
      が成り立つと仮定する

  # 再帰部
    xs = x:xs' のとき
      {左辺} = (x:xs') ++ (ys ++ zs)
             = x : (xs' ++ (ys ++ zs)) ∵ ② 
             = x : ((xs' ++ ys) ++ zs)
      {右辺} = (x:xs' ++ ys) ++ zs
             = x : ((xs ++ ys) ++ zs)

  よって示された
--}


-- 7
map f []     = []
map f (x:xs) = f x : map f xs

(f . g) x    = f (g x)


-- の元で

map f (map g xs) = map (f . g) xs

{--
  であることを証明する

  # 基底部
    xs = [] のとき
    {左辺} = map f (map g [])
           = map f []
           = []
    {右辺} = map (f . g) []
           = []

  # 仮定
    xs = xs' のとき
      map f (map g xs') = map (f . g) xs'
    が成り立つことを仮定する

  # 再帰部
    xs = x:xs' のとき
    {左辺} = map f (map g (x:xs'))
           = map f (g x : map g xs')
           = f (g x) : map f (map g xs')
           = (f . g) x : map (f . g) xs'
    {右辺} = map (f . g) (x:xs')
           = (f . g) x : map (f . g) xs'

  証明終了
--}


-- 8
take 0 _      = []
take n []     = []
take n (x:xs) = x : take (n - 1) xs

drop 0 xs     = xs
drop n []     = []
drop n (_:xs) = drop (n - 1) xs

{-- の元で

  take n xs ++ drop n xs = xs         --(*)

  を証明する

  # 基底部
    n = 0 のとき
      xs = []    のとき
        take 0 [] ++ drop 0 [] = [] ++ []
                               = []
      xs = xs'   のとき
        take 0 xs' ++ drop 0 xs' = xs'
        が成立すると仮定する
      xs = x:xs' のとき
        take 0 (x:xs') ++ dorp 0 (x:xs') = [] ++ (x:xs')
                                         = x:xs'

    よって n = 0 のとき (*) は成り立つ

  # 仮定
    n = k のとき
      take k xs ++ drop k xs = xs
    が成り立つと仮定する

  # 帰納部
    n = k + 1 のとき
      xs = []    のとき
        take (k + 1) [] ++ drop (k + 1) [] = [] ++ []
                                           = []

      xs = xs'   のとき
        take (k + 1) xs' ++ drop (k + 1) xs' = xs'
        が成立すると仮定する

      xs = x:xs' のとき
          take (k + 1) (x:xs') ++ drop (k + 1) xs'
        = x : take k xs'       ++ drop k       xs'
        = x : xs'

  証明終了
--}
