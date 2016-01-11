(<&&>) :: Bool -> Bool -> Bool
(<&&>) x y =    if x == True
                then
                    if y == True
                    then True
                    else False
                else False


(<&|&>) :: Bool -> Bool -> Bool
(<&|&>) x y =   if x == True
                then y
                else
                    if x == False
                    then False
                    else False
