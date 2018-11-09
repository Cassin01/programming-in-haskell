{-- 
-- Parser
return :: a -> Parser a
(>>=) :: Parser a -> (a -> Parser b) -> Parser b

-- IO
return :: a -> IO a
(>>=) :: IO a -> (a -> IO b) -> IO b
--}
--
class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
