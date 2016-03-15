import Actions

getSecretLine :: IO String
getSecretLine = do c <- getCh
                   if c == '\n' then
                       return []
                   else
                       do putChar '-'
                          secret <- getSecretLine
                          return (c:secret)

hangman :: IO ()
hangman = do word <- getSecretLine
             putChar '\n'
             isWin <- guess 5 word
             if isWin then
                 do putStrLn "You won!"
                    return ()
             else
                 do putStrLn "You lost"
                    return ()

diff :: String -> String -> String
diff xs ys = [if elem x ys then x else '-' | x <- xs]

guess :: Int -> String -> IO Bool
guess t secretWord =  do word <- getLine
                         if word == secretWord then
                             return True
                         else
                             do putStrLn (diff secretWord word)
                                let tries = t - 1
                                if tries /= 0 then
                                    do putStr (show(tries))
                                       putStrLn " tries left"
                                       guess tries secretWord
                                else
                                    return False
