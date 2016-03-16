import Actions
import Parser
import Parsing
import Data.List (findIndex, tails, isPrefixOf)

box :: [String]
box =  ["+---------------+",
        "|               |",
        "|               |",
        "+---+---+---+---+",
        "| q | c | d | = |",
        "+---+---+---+---+",
        "| 1 | 2 | 3 | + |",
        "+---+---+---+---+",
        "| 4 | 5 | 6 | - |",
        "+---+---+---+---+",
        "| 7 | 8 | 9 | * |",
        "+---+---+---+---+",
        "| 0 | ( | ) | / |",
        "+---+---+---+---+"]

findSubstringIndex :: String -> String -> Int
findSubstringIndex text pattern
     = case findIndex (pattern `isPrefixOf`) (tails text) of
        Just n -> n
        Nothing -> -1

buttons :: [Char]
buttons = standard ++ extra
          where
            standard = "qcd=123+456-789*0()/"
            extra = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = seqn [writeAt (1, y) xs | (y, xs) <- zip [1..14] box]

fitScreen :: String -> String
fitScreen xs = reverse (take 13 (reverse xs))

display :: String -> IO ()
display xs = do writeAt (2, 2) "               "
                writeAt (2, 2) (fitScreen xs)
                writeAt (2, 3) "               "

displayErr :: String -> IO ()
displayErr xs = do writeAt (2, 3) "               "
                   writeAt (2, 3) xs

calc :: String -> IO ()
calc xs = do c <- getCh
             if elem c buttons then
                 process c xs
             else
                 do beep
                    display xs
                    calc xs

process :: Char -> String -> IO ()
process c xs
    | elem c "qQ\ESC"       = quit
    | elem c "dD\BS\DEL"    = delete xs
    | elem c "=\n"          = evalExp xs
    | elem c "cC"           = clear
    | otherwise             = press c xs

quit :: IO ()
quit = goto (1, 14)

delete :: String -> IO ()
delete "" = do display ""
               calc ""
delete xs = do display (init xs)
               calc (init xs)

replaceTilda :: String -> String
replaceTilda xs = take (length xs) (repeat '~')

prepareErr :: String -> String -> String
prepareErr orig part = (take pos (repeat ' ')) ++ (replaceTilda part)
                       where
                            pos = findSubstringIndex orig part

evalExp :: String -> IO ()
evalExp xs = case parse expr xs of
            [(n, "")] -> do display (show n)
                            calc (show n)
            [(n, part)] -> do displayErr (fitScreen (prepareErr xs part))
                              beep
                              calc xs
                              display xs

clear :: IO ()
clear = do display ""
           calc ""


press :: Char -> String -> IO ()
press c xs = do display (xs ++ [c])
                calc (xs ++ [c])

run :: IO ()
run = do cls
         showbox
         clear
