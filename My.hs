mySucc :: Int -> Int
mySucc x = x + 1

myIsNeg :: Int -> Bool
myIsNeg x
  | x < 0 = True
  | otherwise = False

myAbs :: Int -> Int
myAbs x
  | x < 0 = x * (-1)
  | otherwise = x

myMin :: Int -> Int -> Int
myMin x y
  | x < y = x
  | otherwise = y

myMax :: Int -> Int -> Int
myMax x y
  | x > y = x
  | otherwise = y

myTuple :: a -> b -> (a, b)
myTuple a b = (a, b)

myTruple :: a -> b -> c -> (a, b, c)
myTruple a b c = (a, b, c)

myFst :: (a, b) -> a
myFst (a, b) = a

mySnd :: (a, b) -> b
mySnd (a, b) = b

mySwap :: (a, b) -> (b, a)
mySwap (a, b) = (b, a)

myHead :: [a] -> a
myHead (x:_) = x
myHead [] = error "Empty"

myTail :: [a] -> [a]
myTail (_:x) = x
myTail [] = error "Empty"

myLength :: [a] -> Int
myLength (x:y) = myLength y + 1
myLength [] = 0

myNth :: [a] -> Int -> a
myNth a n
  | n > myLength a = error "Too Large"
  | n < 0 = error "index is negative"
myNth (x:y) 0 = x
myNth (x:y) n = myNth y (n - 1)

myTake :: Int -> [a] -> [a]
myTake n a
  | n > myLength a = a
  | n == myLength a = a
myTake 0 (x:y) = []
myTake n (x:y) = x:myTake (n - 1) y

myDrop :: Int -> [a] -> [a]
myDrop n a
  | n > myLength a = []
myDrop 1 (x:y) = y
myDrop n (x:y) = myDrop (n - 1) y

myAppend :: [a] -> [a] -> [a]
myAppend (a:b) c = a:myAppend b c
myAppend _ b = b

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (a:b) = myAppend (myReverse b) [a]

myInit :: [a] -> [a]
myInit [] = error "Empty"
myInit a = myReverse (myTail (myReverse a))

myLast :: [a] -> a
myLast [] = error "Empty"
myLast a = myHead (myReverse a)

myZip :: [a] -> [b] -> [(a, b)]
myZip [] [] = []
myZip a [] = []
myZip [] b = []
myZip (a:as) (b:bs) = myAppend [(myTuple a b)] (myZip as bs)

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip (a:b) = myTuple(myFst a : myFst(myUnzip b))(mySnd a : mySnd(myUnzip b))


myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (a:b) = f a : myMap f b

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter fun [] = []
myFilter fun (a:b)
  | fun a = a : myFilter fun b
  | otherwise = myFilter fun b

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f b [] = b
myFoldl f b (x:xs) = myFoldl f (f b x) xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f b [] = b
myFoldr f b a@(x:xs) = myFoldr f (f (myHead(myReverse a)) b)
                       (myReverse(myTail(myReverse a)))

myFilter2 :: (a -> Bool) -> [a] -> [a]
myFilter2 fun [] = []
myFilter2 fun (a:b) | fun a == False = a :  myFilter2 fun b
                    | otherwise = myFilter2 fun b

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition f x = myTuple (myFilter f x) (myFilter2 f x)
