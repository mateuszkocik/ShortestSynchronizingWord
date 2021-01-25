module SynchronizingWord(
    buildPowerSet,
    getShortestSynchronizingWord,
    findStateInfo,
    markAndPushIfNotVisited,
    findSSW,
    StateInfo(..)
) where
import Automaton
import Data

type SSW = [Symbol]

convertSSWToString :: SSW -> String
convertSSWToString = foldr1(++)

data StateInfo = EmptySI | StateInfo {state :: State, ancestor :: StateInfo, visited :: Bool} deriving Show

instance Eq StateInfo where
    (==) x y =  stateId (state x) == stateId (state y)

instance Ord StateInfo where
    (StateInfo s1 _ _) `compare` (StateInfo s2 _ _) = s1 `compare` s2


buildTransition :: Automaton -> Symbol -> [Int] -> [Int] -> Maybe(Transition)
buildTransition _ _ [] []                     = Nothing
buildTransition _ sym [] availableS           = Just $ Transition sym (sum availableS)
buildTransition aut sym (next:pos) availableS  
                                                | nextStateIsNothing    = Nothing
                                                | fst wasBefore         = buildTransition aut sym pos availableS
                                                | otherwise             = buildTransition aut sym pos ((snd wasBefore):availableS)
                                            where
                                                singletonStates         = states aut
                                                stateOnPos              = singletonStates !! next
                                                nextStateTransition     = getTransition stateOnPos sym
                                                nextStateIsNothing      = nextStateTransition == Nothing
                                                
                                                wasBefore               = isInAvailableS nextStateTransition
                                                isInAvailableS (Just t) = (t `elem` availableS,t)
                                                
buildTransitions :: Automaton -> [Int] -> [Transition]
buildTransitions aut pos = catMaybes (map (\x -> (buildTransition aut x pos [])) (alphabet aut))

getState :: Automaton -> Int -> State
getState automaton x  = if sSize == 1 then 
                            states automaton !! head positions
                        else 
                            State x sSize (buildTransitions automaton positions)
                        where 
                            bits      = (convertBin x)
                            positions = getPositions bits
                            sSize     = length positions

createNewStateInfo :: State -> StateInfo
createNewStateInfo s = StateInfo s EmptySI False

buildPowerSet :: Automaton -> Tree StateInfo
buildPowerSet aut = buildTreeFromSortedList (map (createNewStateInfo . getState aut) [1..powerSetSize])
                        where powerSetSize = 2^(automatonSize aut) - 1

findStateInfo :: Tree StateInfo -> Int -> StateInfo
findStateInfo (Node a l r) x 
                            | x == s = a
                            | x > s  = findStateInfo r x
                            | x < s  = findStateInfo l x
                            where s = stateId (state a)


markAndPushIfNotVisited :: Tree StateInfo -> Queue StateInfo -> Int -> StateInfo -> (Tree StateInfo, Queue StateInfo)
markAndPushIfNotVisited (Node a l r) q x anc
            | x == s = if visited a then 
                            (Node a l r, q)
                       else 
                           (Node updatedState l r, push q updatedState)
            | x > s = (Node a l (fst rightResult), snd rightResult)
            | x < s = (Node a (fst leftResult) r, snd leftResult)
            where s = stateId (state a)
                  updatedState = StateInfo (state a) anc True
                  leftResult = markAndPushIfNotVisited l q x anc
                  rightResult = markAndPushIfNotVisited r q x anc

getStatesInfoFromTransitions :: Tree StateInfo -> [Transition] -> [StateInfo]
getStatesInfoFromTransitions pS transit = map(\x -> (findStateInfo pS (toState x))) transit

loop :: Tree StateInfo -> Queue StateInfo -> StateInfo -> [StateInfo] -> (Tree StateInfo, Queue StateInfo)
loop pS q _ [] = (pS,q)
loop pS q anc (x:available) = loop (fst result) (snd result) anc available
                            where
                                result = markAndPushIfNotVisited pS q (stateId (state x))  anc

getTransitionSymbol :: StateInfo -> StateInfo -> Symbol
getTransitionSymbol from to = symbol(head(filter (\x -> (toState x) == (stateId tS)) (transitions fromState)))
                            where
                                fromState = state from
                                tS = state to

getPathFromRoot' :: StateInfo -> StateInfo -> [Symbol] -> [Symbol]
getPathFromRoot' _ EmptySI ssw = ssw
getPathFromRoot' x anc ssw = (getTransitionSymbol anc x) : (getPathFromRoot' anc (ancestor anc) ssw)

getPathFromRoot :: StateInfo -> SSW
getPathFromRoot s = getPathFromRoot' s (ancestor s) []

setAncestor :: StateInfo -> StateInfo -> StateInfo
setAncestor x anc = StateInfo (state x) anc (visited x)

processUntilEmptyQueue :: Tree StateInfo -> Queue StateInfo -> Maybe(SSW)
processUntilEmptyQueue _ EmptyQueue = Nothing
processUntilEmptyQueue powerSet queue = if length singletons == 0 then processUntilEmptyQueue (fst updatedDataStructures) (snd updatedDataStructures)
                                        else Just (getPathFromRoot (setAncestor (head singletons) stateToProcess))
                                    where
                                        popResult       = unsafeCatMaybe (pop queue)
                                        queueAfterPop   = snd popResult
                                        stateToProcess  = fst popResult
                                        available       = getStatesInfoFromTransitions powerSet (transitions (state stateToProcess))
                                        singletons      = filter (\x -> stateSize (state x) == 1) available
                                        updatedDataStructures = loop powerSet queueAfterPop stateToProcess available

findSSW :: Automaton -> Maybe(SSW)
findSSW aut = processUntilEmptyQueue powerSet queue
            where 
                initialStateId = 2 ^ (automatonSize aut) - 1
                markIniResult = markAndPushIfNotVisited (buildPowerSet aut) EmptyQueue initialStateId EmptySI
                powerSet = fst markIniResult
                queue = snd markIniResult

getShortestSynchronizingWord :: Automaton -> String
getShortestSynchronizingWord = returnMessage . findSSW
                            where 
                                returnMessage Nothing = "Automaton not synchronizing"
                                returnMessage (Just x) = (convertSSWToString (reverse x))