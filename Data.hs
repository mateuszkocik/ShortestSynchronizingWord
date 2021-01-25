module Data
(
    catMaybes,
    unsafeCatMaybe,
    Queue(..),
    push,
    pop,
    Bin(..),
    convertInt,
    convertBin,
    Tree(..),
    buildTreeFromSortedList,
    getPositions
) where

catMaybes :: [Maybe a] -> [a]
catMaybes ls = [x | Just x <- ls]

unsafeCatMaybe :: Maybe a -> a
unsafeCatMaybe (Just a) = a

data Queue a = EmptyQueue | Queue [a] [a] deriving Show

push :: Queue a -> a -> Queue a
push EmptyQueue x = Queue [x] []
push (Queue front back) x = Queue front (x:back)

pop :: Queue a -> Maybe (a,Queue a)
pop EmptyQueue = Nothing
pop (Queue [] back) = pop $ Queue (reverse back) []
pop (Queue (value:frontRest) back) = Just (value, reducedQueue)
                                 where reducedQueue = if null frontRest && null back then EmptyQueue
                                                      else Queue frontRest back

-- No Digits Binary
data Bin = NDB | Zero Bin | One Bin

instance Show Bin where
    show NDB = ""
    show (Zero x) = show x ++ "0"
    show (One x) = show x ++ "1"


--convert functions for Bin in reversed order digits e.g. 1011 = 13
convertInt :: Bin -> Int
convertInt = decode (1,0)
            where decode (_,v) NDB = v
                  decode (pos,v) (Zero rest) = decode (pos*2,v) rest
                  decode (pos,v) (One rest) = decode (pos*2,v+pos) rest

convertBin :: Int -> Bin
convertBin x = encode NDB x
            where encode b 0 = b
                  encode b int = if int `mod` 2 == 1 then (One $ encode b (int `div` 2))
                                 else (Zero $ (encode b (int `div` 2)))

getPositions :: Bin -> [Int]
getPositions bin = getPositions' bin 0 []
                where getPositions' NDB _ acc = acc
                      getPositions' (One rest) v acc = getPositions' rest (v+1) (v:acc)
                      getPositions' (Zero rest) v acc = getPositions' rest (v+1) acc

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving Show

buildTreeFromSortedList :: (Ord a) => [a] -> Tree a
buildTreeFromSortedList [] = EmptyTree
buildTreeFromSortedList list = Node (list !! half) 
                                    (buildTreeFromSortedList (take half list)) 
                                    (buildTreeFromSortedList (drop (half+1) list))
                            where 
                                half = length list `quot` 2


