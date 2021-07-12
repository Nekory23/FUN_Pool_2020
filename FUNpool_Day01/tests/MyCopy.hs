module My where

-- Step 1
-- Task 01
mySucc :: Int -> Int
mySucc x = x + 1

-- Task 02
myIsNeg :: Int -> Bool
myIsNeg x = if x < 0
            then True
            else False

-- Task 03
myAbs :: Int -> Int
myAbs x = if myIsNeg x == True
          then -x
          else x

-- Task 04
myMin :: Int -> Int -> Int
myMin x y = if x < y
            then x
            else y

-- Task 05
myMax :: Int -> Int -> Int
myMax x y = if x > y
            then x
            else y

-- Step 2
-- Task 06
myTuple :: a -> b -> (a, b)
myTuple a b = (a, b)

-- Task 07
myTruple :: a -> b -> c -> (a, b, c)
myTruple a b c = (a, b, c)

-- Task 08
myFst :: (a, b) -> a
myFst (a, b) = a

-- Task 09
mySnd :: (a, b) -> b
mySnd (a, b) = b

-- Task 10
mySwap :: (a, b) -> (b, a)
mySwap (a, b) = (b, a)

-- Step 3
-- Task 11
myHead :: [a] -> a
myHead [] = error "The list is empty"
myHead (a:b) = a

-- Task 12
myTail :: [a] -> [a]
myTail [] = error "The list is empty"
myTail (a:b) = b;

-- Task 13
myLength :: [a] -> Int
myLength [] = 0
myLength (a:b) = 1 + myLength b

-- Task 14
myNth :: [a] -> Int -> a
myNth [] n = error "The list is empty"
myNth (a:b) 0 = a
myNth (a:b) n 
    | n > myLength b = error "The index is too big"
    | n < 0 = error "The index must be positive number"
    | n > 0 = myNth b (n - 1)

-- Task 15
myTake :: Int -> [a] -> [a]
myTake n [] = error "The list is empty"
myTake 0 (a:b) = []
myTake n (a:b) 
    | n >= myLength b = (a:b)
    | n < 0 = error "The index must be positive"
    | n > 0 = a : myTake (n - 1) b

-- Task 16
myDrop :: Int -> [a] -> [a]
myDrop n [] = error "the list is empty"
myDrop 0 (a:b) = (a:b)
myDrop n (a:b) 
    | n < 0 = error "The index must be positive"
    | n >= myLength b = []
    | n > 0 = myDrop (n - 1) b

-- Task 17
myAppend :: [a] -> [a] -> [a]
myAppend (a:b) c = a : myAppend b c
myAppend [] c = c

-- Task 18
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (a:b) = myAppend(myReverse b) [a]

-- Task 19
myInit :: [a] -> [a]
myInit [] = error "The list is empty"
myInit list@(a:[]) = []
myInit list@(a:b) = a : myInit b

-- Task 20
myLast :: [a] -> a
myLast [] = error "The list is empty"
myLast list@(a:[]) = a
myLast list@(a:b) = myLast b

-- Task 21
myZip :: [a] -> [b] -> [(a, b)]
myZip [] [] = []
myZip a [] = []
myZip [] b = []
myZip list@(a:b) listb@(c:d) = myAppend[(a, c)] (myZip b d)

-- Task 22
myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip ((a, b):[]) = ([a], [b])
myUnzip ((a, b):c) = (a :(myFst (myUnzip c)), b :(mySnd (myUnzip c)))

-- Step 4
-- Task 23
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap func (a:b) = func a : myMap func b

-- Task 24
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter func (a:b) = if func a == True
                      then a : myFilter func b
                      else myFilter func b

-- Task 25
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl func a [] = a
myFoldl func a (b:bs) = myFoldl func (func a b) bs

-- Task 26
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr func a [] = a
myFoldr func a list = myFoldr func (func (myLast list) a) (myInit list)

-- Task 27
myUnfilter :: (a -> Bool) -> [a] -> [a]
myUnfilter _ [] = []
myUnfilter func (a:b) = if func a == False
                      then a : myUnfilter func b
                      else myUnfilter func b

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition _ [] = ([], [])
myPartition func list = myTuple (myFilter func list) (myUnfilter func list)

-- Task 28
--myQuickSort :: (a -> a -> Bool) -> [a] -> [a]
--myQuickSort _ [] = []
--myQuickSort func (a:as)