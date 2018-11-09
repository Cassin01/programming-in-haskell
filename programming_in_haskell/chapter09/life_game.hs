width :: Int
width  = 5

height :: Int
height  = 5

type Board = [Pos]

glider :: Board
glider [(4,2),(2,3),(4,3),(3,4),(4,4)]

showcells :: Board -> IO ()
showcells b = seqn [writeat p "o" | p <- b]

isAlie :: Board -> Pos -> Bool
isAlie b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlie b p)

neihbs :: Pos -> [Pos]
neihbs (x, y) = map wrap [(x - 1, y - 1)
                         ,(x    , y - 1)
                         ,(x + 1, y - 1)
                         ,(x - 1, y    )
                         ,(x + 1, y    )
                         ,(x - 1, y + 1)
                         ,(x    , y + 1)
                         ,(x + 1, y + 1)]

wrap :: Pos -> Pos
wrap (x, y) = (((x - 1) `mod` width) + 1,
               ((y - 1) `mod` height) + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlie b) . neihbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2, 3]]

births :: Board -> [Pos]
births b = [(x, y) | x <- [1..width], 
                     y <- [1..width],
                     isEmpty b (x, y),
                     liveneighbs b (x, y) == 3]

births b = [p | p <- rmdups (concat (map neihbs b)),
                isEmpty b p,
                liveneighbs b p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

life :: Board -> IO ()
life b = do cls
            showells b
            wait 5000
            life (nextgen b)

wait :: Int -> IO ()
wait n = seqn [return () | _ <- [1..n]]
