# Shortest Synchronizing Word

Application for finding shortest synchronizing word (reset word) in given DFA written in Haskell.

## File format
First line must consist DFA alphabet where symbols are separated by comma, in following lines are state descriptions where first number in each line define state name (states must be numbered from 0 to n-1) and then are transitions separated by space.

```
a,b,c,d
0 (b,0) (a,1) (c,3) (d,2)
1 (a,1) (c,1) (b,0) (d,0)
2 (d,1) (c,0) (a,2) (b,3)
3 (b,3) (d,3) (a,2) (c,2)
```
File examples are in data folder.

## Setup
```
git clone https://github.com/mateuszkocik/ShortestSynchronizingWord.git
cd ShortestSynchronizingWord
ghc Main.hs
./Main filename
```

## Technologies
- GHC 8.8.4 
