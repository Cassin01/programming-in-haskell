import Prelude hiding(return, (>>=), (>>))
import Data.Char

type Parser a = String -> [(a, String)]

return :: a -> Parser a
return v = (\inp -> [(v, inp)])

item :: Parser Char
item = (\inp -> case inp of [] -> []
                            (x:xs) -> [(x, xs)])
failure :: Parser a
failure = (\inp -> [])

parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp

(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f = \inp -> case parse p inp of
                    [] -> []
                    [(v, out)] -> parse (f v) out

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \inp -> case parse p inp of
                    [] -> parse q inp
                    [(v, out)] -> [(v, out)]

sat :: (Char -> Bool) -> Parser Char
sat p = item >>= \x -> if p x then return x else failure

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum
         
char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string [] = return []
string (x:xs) = char x >>= (\_ -> string xs >>= (\_ -> return (x:xs)))

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = p >>= (\v -> many p >>= (\vs -> return (v:vs)))

ident :: Parser String
ident = lower >>= (\x -> many alphanum >>= (\xs -> return (x:xs)))

nat :: Parser Int
nat = many1 digit >>= (\xs -> return (read xs))

space :: Parser ()
space = many (sat isSpace) >>= (\_ -> return ())

token :: Parser a -> Parser a
token p = space >>= \_ ->
          p     >>= \v ->
          space >>= \_ ->
          return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token $string xs

p :: Parser [Int]
p = symbol "["                        >>= \_ ->
    natural                           >>= \n ->
    many (symbol "," >>= (\_ -> natural)) >>= \ns ->
    symbol "]"                        >>= \_ -> 
    return (n:ns)

--------------------------------------------------------
expr2 :: Parser Int
expr2 = (expr2 >>= \e ->
         (symbol "-" >>= \_ ->
          natural >>= \n ->
          return (e - n))
         +++ return e)
        +++ natural

expr3 :: Parser Int
expr3 = natural >>= \v ->
        many (symbol "-" >>= \_ ->
              natural    >>= \n ->
              return n) >>= \ss ->
        return (foldl (-) v ss)

expr :: Parser Int
expr = term       >>= \t ->
       (symbol "+" >>= \_ ->
        expr       >>= \e ->
        return (t + e))
       +++ (many (symbol "-" >>= \_ ->
                  term       >>= \n ->
                  return n)  >>= \ss ->
            return (foldl (-) t ss))
       +++ return t

term :: Parser Int
term = power     >>= \f ->
            (symbol "*" >>= \_ ->
             term       >>= \t ->
             return (f * t))
            +++ (symbol "/" >>= \_ ->
                 term       >>= \t ->
                 return (f `div` t))
       +++ return f

power :: Parser Int
power = factor >>= \f ->
             (symbol "^" >>= \_ ->
              power      >>= \p ->
              return (f ^ p))
        +++ return f
           
factor :: Parser Int
factor = (symbol "(" >>= \_ ->
          expr       >>= \e ->
          symbol ")" >>= \_ ->
          return e
         )
         +++ natural
                 
eval :: String -> Int
eval xs = case parse expr xs of
            [(n,[])] -> n
            [(_, out)] -> error ("unused input" ++ out)
            [] -> error "invalid input"


----------------------------
int :: Parser Int
int = (symbol "-" >>= \_ ->
       natural    >>= \i ->
       return (-1 * i))
      +++ natural

untilBreak :: Parser ()
untilBreak = many (sat (/= 'n')) >>= \_ ->
             char '\n' >>= \_ -> return ()

comment :: Parser ()
comment = string "--" >>= \_ ->
          untilBreak >>= \_ ->
          return ()