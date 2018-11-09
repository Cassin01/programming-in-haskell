import Data.List
--bm1 = [f x | x <- xs, p x]

bm2 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
bm2 f p = map f . filter p

myall :: (a -> Bool) -> [a] -> Bool
myall f [] = True
myall f (x:xs) = f x && (myall f xs)

myany :: (a -> Bool) -> [a] -> Bool
myany f [] = False
myany f (x:xs) = f x || (myany f xs)

mytakeWhile :: (a -> Bool) -> [a] -> [a]
mytakeWhile f [] = []
mytakeWhile f (x:xs)
    | f x == True = x : mytakeWhile f xs
    | otherwise   = []


mydropWhile :: (a -> Bool) -> [a] -> [a]
mydropWhile f [] = []
mydropWhile f (x:xs)
    | f x == True = mydropWhile f xs
    | otherwise   = xs

map'' :: (a -> b) -> [a] -> [b]
map'' f [] = []
map'' f (x:xs) = f x : (map'' f xs)

foldrmap :: (a -> b) -> [a] -> [b]
foldrmap f  = foldr (\ x xs -> f x : xs) []

takedrop :: (a -> Bool) -> a -> [a]
takedrop f a
  | f a == True = [a]
  | otherwise   = []

foldrfilter :: (a -> Bool) -> [a] -> [a]
foldrfilter f = foldr (\ x xs -> (takedrop f x) ++ xs) []

-- 4

--dec2int :: [Int] -> [(Int, Int)]
dec2int :: [Int] -> Int
dec2int xs = foldl (\ a (f, b) -> a + f * 10 ^ b) 0 [(x, y) | (x, y) <- zip xs [(len - 1), (len - 2) .. ]]
  where len = length xs

-- 5
id' :: a -> a
id' = \ x -> x

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id'

-- work
sumsqreven = compose [map (^2), filter even]

{--
'sumsqreven = compose [sum, map (^2), filter even]' isn't work because
functions have different numbers of parameter.
--}

-- 6
add' :: Int -> Int -> Int
add' x y = x + y

curry' :: (a -> b -> c) -> (a -> (b -> c))
curry' f = \ x -> (\ y -> f x y)

-- *Main> (curry' add') 2 1

uncurry' :: (a -> (b -> c)) -> (a -> b -> c)
uncurry' f = \ x y -> f x y

-- *Main> (uncurry' add') 2 1

-- 7
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

-- int2bin
type Bit = Int

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

int2bin' = unfold (== 0) (`mod` 2) (`div` 2)

-- chop8
chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

chop8' = unfold (null) (take 8) (drop 8)


-- map f

mapf :: (a -> b) -> [a] -> [b]
mapf f [] = []
mapf f (x:xs) = (f x) : (mapf f xs)

mapf' :: (a -> b) -> [a] -> [b]
mapf' f xs = unfold (null) (f . head) (drop 1) xs


-- iterate f
{--
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)
--}

iteratef :: (a -> a) -> a -> [a]
iteratef f x = x : iterate f (f x)

rF :: a -> Bool
rF _ = False

iteratef' :: (a -> a) -> a -> [a]
iteratef' f = unfold rF id f

 
