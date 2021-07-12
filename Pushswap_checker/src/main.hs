import System.Environment
import System.Exit
import Data.List
--import Systems
import Commands

-- is the list sorted ? --
isListSorted :: [Int] -> Bool
isListSorted list
    | list == sort list = True
    | otherwise = False

-- execute the commands on the lists --
execCmdsBb :: [String] -> [Int] -> [Int] -> IO ()
execCmdsBb [] l_a _ = if isListSorted l_a
                        then putStrLn "OK" >> return ()
                        else putStrLn "KO" >> return ()
execCmdsBb (command:cs) l_a l_b = case command of
    "pb" -> execCmds cs (fst (pb (l_a, l_b))) (snd (pb (l_a, l_b)))
    "ra" -> execCmds cs (r_ab l_a) l_b
    "rb" -> execCmds cs l_a (r_ab l_b)
    _ -> exitWith (ExitFailure 84)

execCmdsB :: [String] -> [Int] -> [Int] -> IO ()
execCmdsB [] l_a _ = if isListSorted l_a
                        then putStrLn "OK" >> return ()
                        else putStrLn "KO" >> return ()
execCmdsB (command:cs) l_a l_b = case command of
    "rr" -> execCmds cs (r_ab l_a) (r_ab l_b)
    "rra" -> execCmds cs (rr_ab l_a) l_b
    "rrb" -> execCmds cs l_a (rr_ab l_b)
    "rrr" -> execCmds cs (rr_ab l_a) (rr_ab l_b)
    _ -> execCmdsBb (command:cs) l_a l_b

execCmds :: [String] -> [Int] -> [Int] -> IO ()
execCmds [] l_a _ = if isListSorted l_a
                        then putStrLn "OK" >> return ()
                        else putStrLn "KO" >> return ()
execCmds (command:cs) l_a l_b = case command of
    "sa" -> execCmds cs (swap l_a) l_b
    "sb" -> execCmds cs l_a (swap l_b)
    "sc" -> execCmds cs (swap l_a) (swap l_b)
    "pa" -> execCmds cs (fst (pa (l_a, l_b))) (snd (pa (l_a, l_b)))
    _ -> execCmdsB (command:cs) l_a l_b

-- ERROR HANDLING --
-- check number input --
checkNumber :: [Char] -> Bool
checkNumber [] = False
checkNumber (a:[]) = if a >= '0' && a <= '9' || a == '-'
                           then True
                           else False
checkNumber (a:as) = if a >= '0' && a <= '9' || a == '-'
                           then checkNumber as
                           else False

checkNumbers :: [String] -> IO ()
checkNumbers [] = exitWith (ExitFailure 84)
checkNumbers (a:[])
    | checkNumber a == True = return ()
    | otherwise = exitWith (ExitFailure 84)
checkNumbers (a:as)
    | checkNumber a == True = checkNumbers as
    | otherwise = exitWith (ExitFailure 84)

-- check command input --
--checkCommands :: [String] -> IO ()
--checkCommands [] = return ()
--checkCommands (start : rest)
--    | start == "sa" || start == "sb" || start == "sc" || start == "pa"
--        || start == "pb" || start == "ra" || start == "rb"
--        || start == "rr" || start == "rra" || start == "rrb"
--       || start == "rrr" = checkCommands rest
--    | otherwise = exitWith (ExitFailure 84)

-- MAIN --
main :: IO ()
main = do
    str <- getLine
    let commands = words str
--    checkCommands commands
    arg <- getArgs
    checkNumbers arg
    let numbers = map read arg :: [Int]
    if isListSorted numbers && commands == []
        then putStrLn "OK"
        else execCmds commands numbers []