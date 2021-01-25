import System.Environment
import Automaton
import SynchronizingWord

import Data

split :: String -> Char -> [String]
split [] _ = [""]
split (x:xs) regex 
             | x == regex = "" : rest
             | otherwise  = (x : head rest) : tail rest
              where rest = split xs regex

parseTransition :: String -> Transition
parseTransition bracket = Transition symbol state
                          where 
                            splittedBracket = split bracket ','
                            symbol = tail $ head splittedBracket
                            state = 2^(read $ init $ last splittedBracket)

parseState :: String -> State
parseState line = State (2^(read $ head splittedLine)) 1 (map (parseTransition) (tail splittedLine))
                        where splittedLine = split line ' '

parseAlphabet :: String -> [Symbol]
parseAlphabet = flip split ','

parseAutomaton :: [String] -> Automaton
parseAutomaton (alphabetLine:stateLines) = Automaton alphabet size states
                                          where 
                                            alphabet = parseAlphabet alphabetLine
                                            states   = map parseState stateLines
                                            size     = length states

main :: IO ()
main = do
  (fileName : _) <- getArgs
  fileContent <- readFile fileName
  let fileLines = lines fileContent
      automaton = parseAutomaton fileLines
      shortestSynchronizingWord = getShortestSynchronizingWord automaton
  print (shortestSynchronizingWord)