-- Task 13
myNth :: Int -> String -> String
myNth n [] = error "The list is empty"
myNth 0 (a:b) = a
myNth n (a:b) 
    | n > length b = error "The index is too big"
    | n < 0 = error "The index must be positive number"
    | n > 0 = myNth b (n - 1)

checkcompute :: String -> Int -> Bool
checkcompute "/" 0 = False
checkcompute "/" b = True
checkcompute "%" 0 = False
checkcompute "%" b = True

checkoperator :: String -> Bool
checkoperator op = if op == "+" || op == "-" || op == "*" 
                     || op == "/" || op == "%"
                  then True
                  else False

compute :: Int -> String -> Int -> Int
compute a op b
    | op == "+" = a + b
    | op == "-" = a - b
    | op == "*" = a * b
    | op == "/" = quot a b
    | otherwise = mod a b

main :: IO ()
main = do 
    args <- getArgs
    if length args == 3 && checkoperator (myNth 1 args) == True &&
        checkcompute (myNth 1 args) (read (myNth 2 args)) == True
    then putStrLn (compute (read (myNth 0 args)) (myNth 1 args) 
         (read (myNth 2 args)))
    else exitWith (ExitFailure 84)
    return ()