import System.Environment
import System.Exit
-- Step 0 : prerequisite
-- Task 01
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (a:[]) = x == a
myElem x (a:as) = if x == a
                       then True
                       else myElem x as

-- Step 1 : Maybe
-- Task 02
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (div x y)

-- Task 03
safeNth :: [a] -> Int -> Maybe a
safeNth [] _ = Nothing
safeNth (a:_) 0 = Just a
safeNth list@(_:as) n
    | n > length list = Nothing
    | n < 0 = Nothing
    | n > 0 = safeNth as (n - 1)

-- Task 04
safeSucc :: Maybe Int -> Maybe Int
safeSucc Nothing = Nothing
safeSucc (Just x) = Just (x + 1)

-- Task 05
myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup _ [] = Nothing
myLookup nbr ((a, b):c)
    | nbr == a = Just b
    | otherwise = myLookup nbr c

-- Task 06
maybeDo :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeDo _ Nothing _ = Nothing
maybeDo _ _ Nothing = Nothing
maybeDo func (Just x) (Just y) = Just (func x y)

-- Task 07
testInt :: [Char] -> Bool
testInt [] = False
testInt (a:[]) = if a >= '0' && a <= '9' || a == '-'
                 then True
                 else False
testInt (a:as) = if a >= '0' && a <= '9' || a == '-'
                 then testInt as
                 else False

readInt :: [Char] -> Maybe Int
readInt list
    | testInt list == True = Just (read list)
    | otherwise = Nothing

-- Step 2
-- Task 08
getLineLength :: IO Int
getLineLength = do
    line <- getLine
    return (length line)

-- Task 09
printAndGetLength :: String -> IO Int
printAndGetLength str = putStrLn str >> return (length str)

-- Task 10
printFirstLast :: Int -> Int -> IO ()
printFirstLast _ 0 = putStrLn "+" >> return()
printFirstLast stock len
    | stock == len = putStr "+" >> printFirstLast stock (len - 1)
    | otherwise = putStr "-" >> printFirstLast stock (len - 1)

printOtherLines :: Int -> Int -> Int -> IO ()
printOtherLines _ 0 0 = putStrLn "|" >> return ()
printOtherLines stock 0 it = putStrLn "|" >> printOtherLines stock stock (it-1)
printOtherLines stock a it
    | stock == a = putStr "|" >> printOtherLines stock (a - 1) it
    | otherwise = putStr " " >> printOtherLines stock (a - 1) it

printBox :: Int -> IO ()
printBox 1 = putStrLn "++"
printBox 2 = printFirstLast 3 3 >> printFirstLast 3 3
printBox a
    | a <= 0 = return()
    | otherwise = printFirstLast (a*2 - 1) ((a*2) - 1) 
                  >> printOtherLines (a*2-1) (a*2-1) (a-3)
                  >> printFirstLast (a*2 - 1) ((a*2) - 1) >> return()

-- Task 11
concatLines :: Int -> IO String
concatLines 0 = return ("")
concatLines nbr = do
        line <- getLine
        lines <- concatLines (nbr - 1)
        return (line ++ lines)

-- Task 12
getInt :: IO (Maybe Int)
getInt = do
    line <- getLine
    if testInt line == True 
    then return (Just (read line))
    else return (Nothing)

-- Task 13
main :: IO ()
main = do 
    args <- getArgs
    return ()