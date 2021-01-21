import System.Environment
import Automaton

split :: String -> Char -> [String]
split [] _ = [""]
split (x:xs) regex 
             | x == regex  = "" : rest
             | otherwise = (x : head rest) : tail rest
              where rest = split xs regex

parseTransition :: String -> Transition
parseTransition bracket = Transition symbol state
                          where splittedBracket = split bracket ','
                                symbol = show $ tail $ head splittedBracket
                                state = read $ init $ last splittedBracket

parseState :: String -> State
parseState line = State (read $ head splittedLine)  (map (parseTransition) (tail splittedLine))
                        where splittedLine = split line ' '

parseAlphabet :: String -> [Symbol]
parseAlphabet = flip split ','

parseAutomaton :: [String] -> Automaton
parseAutomaton (alphabetLine:stateLines) = Automaton alphabet states size
                                          where alphabet = parseAlphabet alphabetLine
                                                states = map parseState stateLines
                                                size = length states

main :: IO ()
main = do
  (fileName : _) <- getArgs
  fileContent <- readFile fileName
  let fileLines = (lines fileContent)
  print (parseAutomaton fileLines)