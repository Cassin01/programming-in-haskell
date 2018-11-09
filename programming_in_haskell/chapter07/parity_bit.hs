type Bit = Int

parity_bit :: [Bit] -> Bit 
parity_bit bits
  | odd $ sum $ filter (==1) bits = 1
  | otherwise                     = 0
