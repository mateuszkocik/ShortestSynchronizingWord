module ShortestSynchronizingWord(
    buildTransition
) where
import Automaton
import Data


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
                                                
