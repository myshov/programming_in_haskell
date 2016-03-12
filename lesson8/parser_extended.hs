import Parsing

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
           +++ do symbol "-"
                  e <- expr
                  return (t - e)
           +++ return t

term :: Parser Int
term = do fe <- factorExp
          do symbol "*"
             t <- term
             return (fe * t)
           +++ do symbol "/"
                  t <- term
                  return (fe `div` t)
           +++ return fe

factorExp :: Parser Int
factorExp = do f <- factor
               do symbol "^"
                  fe <- factorExp
                  return (f ^ fe)
                +++ return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         +++ natural

eval :: String -> Int
eval xs = case parse expr xs of
               [(n, [])] -> n
               [(_, out)] -> error("unused input " ++ out)
               [] -> error "invalid input"
