module Data
(
    Queue(..)
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

