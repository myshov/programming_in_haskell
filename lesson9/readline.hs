readLine :: IO String
readLine = get ""

get      :: String -> IO String
get xs   = do x <- getChar
              if x == '\n' then
                  return xs
              else if x == '\DEL' then
                  if null xs then
                      do putStr "\ESC[3D   \ESC[3D"
                         get ""
                  else
                      do putStr "\ESC[3D   \ESC[3D"
                         get (init xs)
              else
                  do get (xs ++ [x])
