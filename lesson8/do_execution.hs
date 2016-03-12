import Parsing

p :: Parser [Int]
p = do symbol "["
       n <- natural
       ns <- many (do symbol ","
                      natural)
       symbol "]"
       return (n:ns)
