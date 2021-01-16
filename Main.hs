import System.Environment

split :: String -> Char -> [String]
split [] _ = [""]
split (x:xs) regex 
             | x == regex  = "" : rest
             | otherwise = (x : head rest) : tail rest
              where rest = split xs regex

getTransitionsFromBracket :: String -> (String, String)
getTransitionsFromBracket bracket = (state, symbol)
                        where splittedBracket = split bracket ','
                              state = tail $ head splittedBracket
                              symbol = init $ last splittedBracket

getStateDescription :: String -> (String,[(String,String)])
getStateDescription line = (head splittedLine, map (getTransitionsFromBracket) (tail splittedLine))
                        where splittedLine = split line ' '

getAutomatonDescription :: [String] -> [(String,[(String,String)])]
getAutomatonDescription line = map (getStateDescription) line

main :: IO ()
main = do
  (fileName : _) <- getArgs
  fileContent <- readFile fileName
  let fileLines = (lines fileContent)
  print (getAutomatonDescription fileLines)