module Data
(
    Queue(..),
    push,
    pop,
    Bin(..),
    convertInt,
    convertBin
) where


data Queue a = Empty | Queue [a] [a]

push :: Queue a -> a -> Queue a
push Empty x = Queue [x] []
push (Queue front back) x = Queue front (x:back)

pop :: Queue a -> Maybe (a,Queue a)
pop Empty = Nothing
pop (Queue [] back) = pop $ Queue (reverse back) []
pop (Queue (value:frontRest) back) = Just (value, reducedQueue)
                                 where reducedQueue = if null frontRest && null back then Empty
                                                      else Queue frontRest back

-- No Digits Binary
data Bin = NDB | Zero Bin | One Bin 

instance Show Bin where
    show NDB = ""
    show (Zero x) = show x ++ "0"
    show (One x) = show x ++ "1"


--convert functions for Bin in reversed order digits e.g. 1011 = 13

convertInt :: Bin -> Integer
convertInt = decode (1,0)
            where decode (_,v) NDB = v
                  decode (pos,v) (Zero rest) = decode (pos*2,v) rest
                  decode (pos,v) (One rest) = decode (pos*2,v+pos) rest

convertBin :: Integer -> Bin
convertBin x = encode NDB x
            where encode b 0 = b
                  encode b int = if int `mod` 2 == 1 then (One $ encode b (int `div` 2))
                                 else (Zero $ (encode b (int `div` 2)))
