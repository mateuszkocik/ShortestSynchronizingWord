module Automaton
(
    Symbol,
    Transition(..),
    State(..),
    StateID,
    Automaton(..),
    getTransition
) where

type Symbol = String

type StateID = Int

data Transition = Transition {symbol :: Symbol,
                              toState :: StateID}

instance Show Transition where
    show x = "(" ++ symbol x ++ "," ++ show (toState x) ++ ")"



data State = State {stateId :: StateID,
                    stateSize :: Int,
                    transitions :: [Transition]}

instance Show State where
    show x = "Id = " ++ show (stateId x) ++ " Size = " ++ show (stateSize x) ++ "   " ++ show (transitions x) ++ "\n"

instance Eq State where
    (==) x y = stateId x == stateId y

instance Ord State where
    (State s1 _ _) `compare` (State s2 _ _) = s1 `compare` s2

getTransition :: State -> Symbol -> Maybe (Int)
getTransition s a = getTransition' (transitions s) a
                  where
                      getTransition' [] _ = Nothing
                      getTransition' (x:xs) a = if (symbol x == a) then 
                                                    Just $ toState x
                                                else 
                                                    getTransition' xs a

data Automaton = Automaton {alphabet :: [Symbol],
                            automatonSize :: Int,
                            states :: [State]}

instance Show Automaton where
    show x = "Alphabet: " ++ show (alphabet x) ++ "\nSize: " ++ show (automatonSize x) ++ "\n"  ++ show (states x) 

