import System.Environment

getEdges :: String -> [(Char,Char)]
getEdges [] = []
getEdges (_:_:state:_:word:_:rest) = (state,word) : getEdges rest

getFromLine :: String -> (Char,[(Char,Char)])
getFromLine (state:rest) = (state,(getEdges rest))

getList :: [String] -> [(Char,[(Char,Char)])]
getList = map getFromLine

main :: IO ()
main = do
  (fileName : _) <- getArgs
  fileContent <- readFile fileName
  let fileLines = (lines fileContent)
  print (getList fileLines)