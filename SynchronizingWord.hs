module ShortestSynchronizingWord(
    buildPowerSet
) where
import Automaton
import Data

--improvement dont call build transition for separate symbol but for alphabet
buildTransition :: Automaton -> Symbol -> [Int] -> [Int] -> Maybe(Transition)
buildTransition _ _ [] []  = Nothing
buildTransition _ sym [] availableS  = Just $ Transition sym (sum availableS)
buildTransition aut sym (next:pos) availableS = if fst wasBefore then
                                                buildTransition aut sym pos availableS
                                                else buildTransition aut sym pos ((snd wasBefore):availableS)
                                            where
                                                singletonStates = states aut
                                                stateOnPos = singletonStates !! next
                                                nextStateTransition = getTransition stateOnPos sym
                                                wasBefore = isInAvailableS nextStateTransition
                                                isInAvailableS Nothing = (True,-1)
                                                isInAvailableS (Just t) = (t `elem` availableS,t)
                                                
buildTransitions :: Automaton -> [Int] -> [Transition]
buildTransitions aut pos = catMaybes (map (\x -> (buildTransition aut x pos [])) (alphabet aut))

getState :: Automaton -> Int -> State
getState automaton x = if sSize == 1 then states automaton !! head positions
                        else State x sSize (buildTransitions automaton positions)
                        where 
                            bits = (convertBin x)
                            positions = getPositions bits
                            sSize = length positions
                  
-- buildPowerSet :: Automaton -> [State]
-- buildPowerSet automaton = map (getState automaton) [1..(2^(automatonSize automaton) - 1)]

buildPowerSet :: Automaton -> Tree State
buildPowerSet automaton = buildTreeFromSortedList (map (getState automaton) [1..powerSetSize])
                        where powerSetSize = 2^(automatonSize automaton) - 1