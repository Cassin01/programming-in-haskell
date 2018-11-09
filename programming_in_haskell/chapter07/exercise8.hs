import Data.Char
import Data.List

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum [w * b | (w, b) <- zip weights bits]
    where weights = iterate (*2) 1

bin2int' :: [Bit] -> Int
bin2int' = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (enparity . make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits) 

decode :: [Bit] -> String
decode = map ((chr . bin2int) . parity_check) . chop9

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

parity_bit :: [Bit] -> Bit 
parity_bit bits
  | odd (sum bits) = 1
  | otherwise      = 0

enparity :: [Bit] -> [Bit]
enparity bits = parity_bit bits : bits


parity_check :: [Bit] -> [Bit]
parity_check (x:xs)
  | x == parity_bit xs = xs
  | otherwise          = error "parity error"

chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits) 


-- 9
bad_transmit :: String -> String
bad_transmit = decode . bad_channel . encode

bad_channel :: [Bit] -> [Bit]
bad_channel = tail
