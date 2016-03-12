import Parsing

toeol :: Parser String
toeol = do char '\n'
           return ("")
          +++
        do x <- item
           xs <- toeol
           return (x:xs)
          +++
        do return ("")

comment :: Parser ()
comment = do xs <- string "--"
             ys <- toeol
             return ()

comment' :: Parser ()
comment' = do string "--"
              many (sat (/= '\n'))
              return ()
