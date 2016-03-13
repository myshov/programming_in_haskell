module Actions where

import System.IO

type Pos = (Int, Int)

strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The sting has "
            putStr (show (length xs))
            putStrLn " characters"

beep :: IO ()
beep = do putStr "\BEL"

cls :: IO ()
cls = do putStr "\ESC[2J"

goto :: Pos -> IO ()
goto (x, y) = putStr("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeAt :: Pos -> String ->  IO ()
writeAt p xs = do goto p
                  putStr xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = do a
                 seqn as

putStr' :: String -> IO ()
putStr' xs = seqn [putChar x | x <- xs]

getCh :: IO Char
getCh  = do hSetEcho stdin False
            c <- getChar
            hSetEcho stdin True
            return c
