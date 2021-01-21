module Automaton
(
    Symbol,
    Transition(..),
    State(..),
    StateID,
    Automaton(..)
) where

type Symbol = String

type StateID = Int

data Transition = Transition {symbol :: Symbol,
                              toState :: StateID
                              }deriving (Show)

data State = State {stateId :: StateID,
                    transitions :: [Transition]
                    }deriving (Show)


data Automaton = Automaton {alphabet :: [Symbol],
                            states :: [State],
                            size :: Int
                            }deriving (Show)

