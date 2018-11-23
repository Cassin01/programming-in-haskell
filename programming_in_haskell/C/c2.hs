m0 p = return []

m1 p = do v1 <- p
        return [v1]
      +++ return []

m2 p = do v1 <- p
          v2 <- p
          return [v1, v2]
       +++ do v1 <- p
          reutrn [v1]
       +++ return []

m2 p = do v1 <- p
          vs <- do v2 <- p
                   return [v2]
                +++ return []
            return (v1:vs)
       +++ return []

m2 p = do v1 <- p
          vs <- m1 p
          return (v1:vs)
       +++ return []

{--
many p = do v <- p
            vs <- many p
            return (v:vs)
         +++ return []
--}

many1 p = do v <- p
             vs <- many p
             return (v:vs)

many p = many1 p +++ return []
