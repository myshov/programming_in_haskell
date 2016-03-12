import Parsing

expr :: Parser Int
expr = do n <- natural
          ns <- many (do symbol "-"
                         natural)
          return (foldl (-) n ns)

eval :: String -> Int
eval xs = case parse expr xs of
               [(n, [])] -> n
               [(_, out)] -> error("unused input " ++ out)
               [] -> error "invalid input"
