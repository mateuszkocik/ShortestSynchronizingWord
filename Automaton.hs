module Automaton
(
    Symbol,
    Transition(..),
    State(..),
    StateID,
    Automaton(..)
) where

type Symbol = String

data Transition = Transition {symbol :: Symbol,
                              toState :: StateID}

data State = State {stateId :: StateID,
                    transitions :: [Transition]}

type StateID = Int

data Automaton = Automaton {alphabet :: [Symbol],
                            states :: [State],
                            size :: Int}

