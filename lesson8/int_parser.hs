import Parsing

int' :: Parser Int
int' = do x <- char '-' +++ digit
          xs <- many1 digit
          return (read (x:xs))

int'' :: Parser Int
int'' = do x <- char '-'
           n <- nat
           return (-n)
        +++ nat
