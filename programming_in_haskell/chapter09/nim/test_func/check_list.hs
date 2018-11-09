import Data.List
import System.IO

main :: IO ()
main = print . show . bitToBool . checkList $ [-1,-3,-4]

--bitToBool :: Int -> Bool
bitToBool :: Int -> Bool
bitToBool = \ x -> if x == 1 then True else False

--checkList :: Foldable t => t Int -> Int
checkList :: Foldable t => t Int -> Int
checkList xs = foldr underZero 0 xs

--underZero :: Int -> Int -> Int
underZero :: Int -> Int -> Int
underZero = \ x y -> if (x <= 0  && y <= 0) then 1 else 0
