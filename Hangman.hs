--Kalil Black
--A program with Traditional and Evil Hangman

module Main where
import System.Environment
import System.Directory
import System.IO
import Data.List
import Data.Char

--Converts string to list of words
toList :: String -> [String]
toList input = (words input)

--Generates Blanks for words
initBlanks :: String -> String
initBlanks "" = ""
initBlanks s = "_ " ++ initBlanks (tail s)

--Regenerates blanks for words
updateBlanks :: Char-> String -> String -> String
updateBlanks c s1 s2
  |null s2 = ""
  |not(elem (toUpper c) s2) = s1
  |otherwise = updtHelper (toUpper c) s1 s2

--Processes the blank updates
updtHelper :: Char-> String -> String -> String
updtHelper c s1 s2
  |null s1 = s1
  |head s2 == c = [c] ++ " " ++ updtHelper c (tail (tail s1)) (tail s2)
  |head s1 == '_' = "_ " ++ 
   updtHelper c (tail (tail s1)) (tail s2)
  |otherwise = [head s1] ++ [head (tail s1)] ++ 
   updtHelper c (tail (tail s1)) (tail s2)

--Adds spaces between string characters
addSpaces :: String -> String
addSpaces "" = ""
addSpaces s1 = [head s1] ++ " " ++ addSpaces (tail s1)

--Checks if word is the solution
isSolution :: String -> Bool
isSolution s1
  |null s1 = True
  |head s1 == '_' = False
  |otherwise = isSolution (tail (tail s1))

--processes each turn and gets user inputs
hangmant solution initWord allGuesses gWrcnt = do
  putStrLn ("Number of incorrect guesses: " ++ show gWrcnt)
  putStrLn "Enter character"
  ch1 <- getLine
  putStrLn ""
  let ch = toUpper (head ch1)
  let newWord = updateBlanks ch initWord solution
  if elem ch allGuesses
    then do
       putStrLn "You already guessed this, try again"
       putStrLn ("Current word is: " ++ newWord)
       hangmant solution initWord allGuesses gWrcnt
  else if newWord == initWord
    then do 
       putStrLn ("Guesses: " ++ addSpaces (sort (allGuesses ++ [ch])) ) 
       putStrLn ("Current word is: " ++ newWord)
       hmwr solution initWord
        (allGuesses ++ [ch]) (gWrcnt + 1)
  else do 
     putStrLn ("Current word is: " ++ newWord)
     putStrLn ("Guesses: " ++ addSpaces (sort (allGuesses ++ [ch])) )
     hmr solution newWord (allGuesses ++ [ch]) gWrcnt

--processing for bad guesses (lose?)
hmwr solution initWord allGuesses gWrcnt = do
  if gWrcnt == 6
          then putStrLn "You Lose!"
       else
          hangmant solution initWord allGuesses gWrcnt 

--processing for good guess (win?)
hmr solution initWord allGuesses gWrcnt = do
  if isSolution initWord
          then putStrLn "You Win!"
     else
         hangmant solution initWord allGuesses gWrcnt

--Combined Hangman main function
main = do
  args <- getArgs
  if null args
    then do
      putStrLn "Invalid number of arguments to program."
      putStrLn ("usage: Hangman -e dictionaryFileName " ++
       "wordLength numberOfGuesses")
      putStrLn "usage: Hangman -s fileContainingWord"
  else if (length args == 2 && args!!0 == "-s")
    then mainTraditional
  else if ((length args == 4 || length args == 5) && args!!0 == "-e")
    then mainEvil
  else if args!!0 == "-s"
    then do
      putStrLn "Invalid number of arguments to program."
      putStrLn "usage: Hangman -s fileContainingWord"
  else if args!!0 == "-e"
    then do
      putStrLn "Invalid number of arguments to program."
      putStrLn ("usage: Hangman -e dictionaryFileName " ++
       "wordLength numberOfGuesses")
  else do
    putStrLn "Invalid number of arguments to program."
    putStrLn ("usage: Hangman -e dictionaryFileName " ++
     "wordLength numberOfGuesses")
    putStrLn "usage: Hangman -s fileContainingWord"

--Traditional Hangman Function
mainTraditional = do
  putStrLn "Welcome to Hangman"
  putStrLn "After 6 incorrect guesses, you lose"
  putStrLn "Each underscore is a letter in the secret word"
  [mode, inFile] <- getArgs
  inFileHandle <- openFile inFile ReadMode
  inputFile <- readFile inFile
  let solution = map toUpper (head (toList (inputFile)))
  hClose inFileHandle
  let initWord = initBlanks solution
  putStrLn ("Current word is " ++ initWord)
  hangmant solution initWord "" 0

--Kalil Black
--Start of Evil Hangman

--Takes dictionary and returns only words of specified length
trimtoSize :: [String] -> Int -> [String]
trimtoSize slist len
  |null slist = slist
  |length (head slist) == len = [map toUpper (head slist)] ++
   trimtoSize (tail slist) len
  |otherwise = trimtoSize (tail slist) len

--gets list of words that have permutations of chosen character
newLists :: String -> [String] -> [String] -> Char -> [[String]]
newLists gw perms currDict c
  |null perms = [[]]
  |otherwise = [nLHelper gw (head perms) currDict c] ++ 
   newLists gw (tail perms) currDict c

--processes each permutation to check for match
nLHelper :: String -> String -> [String] -> Char -> [String]
nLHelper gw perms currDict c
  |null currDict = []
  |isPermMatch perms (head currDict) gw c = [head currDict] ++
   nLHelper gw perms (tail currDict) c
  |otherwise = nLHelper gw perms (tail currDict) c

--Checks if s String matches pattern in permutation
isPermMatch :: String -> String -> String -> Char -> Bool
isPermMatch currPerm currDictWord currGameWord c
  |null currPerm = True
  |null currDictWord = False
  |head currGameWord == '_' && ((head currPerm == '_' &&
   head currDictWord /= c) || (head currPerm /= '_' &&
   head currDictWord == c)) = isPermMatch (tail currPerm) 
   (tail currDictWord) (tail (tail currGameWord)) c
  |otherwise = isPermMatch currPerm (tail currDictWord) 
   (tail (tail currGameWord)) c

--Gets the largest list of words in list of possible dictionaries
updateDict :: Char -> String -> [String] -> [String]
updateDict ch gw currDict
    |null currDict = []
    |otherwise = upHelp (fullList ++ [nocharstrs]) []
    where
        nocharstrs = removestrs ch currDict
        fullList = newLists gw (charPerms ch (blanks gw)
         (blanks gw)) (containCh ch currDict) ch

--Removes words from list with certain char in it
removestrs :: Char -> [String] -> [String]
removestrs ch currDict
    |null currDict = []
    |elem ch (head currDict) = removestrs ch (tail currDict)
    |otherwise = [head currDict] ++ removestrs ch (tail currDict)

--Determines larges list in list of string lists
upHelp :: [[String]] -> [String] -> [String]
upHelp slists largest
  |null slists = largest
  |length (head slists) > length largest = upHelp (tail slists) (head slists)
  |otherwise = upHelp (tail slists) largest

--Counts blanks in word
blanks :: String -> Int
blanks s
  |null s = 0
  |head s == '_' = 1 + blanks (tail (tail s))
  |otherwise = blanks (tail (tail s))

--returns string list that all has certain char in them
containCh :: Char -> [String] -> [String]
containCh ch sl
  |null sl = sl
  |elem ch (head sl) = [head sl] ++ containCh ch (tail sl)
  |otherwise = containCh ch (tail sl)

--Creates permutations of certain pattern
charPerms :: Char -> Int -> Int -> [String]
charPerms ch currlen tlen
  |currlen < 0 = []
  |otherwise = nub (permutations (initperm ch currlen tlen)) ++ 
   charPerms ch (currlen - 1) tlen

--Creates initial pattern for permutations
initperm :: Char -> Int -> Int -> String
initperm ch currlen tlen
  |tlen < 1 = ""
  |currlen < 1 = "_" ++ initperm ch currlen (tlen - 1)
  |otherwise = [ch] ++ initperm ch (currlen - 1) (tlen - 1)

--Main function for evil hangman, processes inputs
hangman initWord initDict allGuesses gWrcnt prtDictlen max = do
  if prtDictlen
    then putStrLn ("Current Dictionary Size: " ++ 
     show (length initDict) )
  else
    return ()
  putStrLn ("Number of incorrect guesses: " ++ show gWrcnt)
  putStrLn "Enter character"
  ch1 <- getLine
  putStrLn ""
  let ch = toUpper (head ch1)
  let newDict = updateDict ch initWord initDict
  let newWord = updateBlanks ch initWord (head newDict)
  if elem ch allGuesses
    then do
       putStrLn "You already guessed this, try again"
       putStrLn ("Current word is: " ++ initWord)
       hangman initWord initDict allGuesses gWrcnt prtDictlen max
  else if null newDict
    then do 
       putStrLn "Bad guess"
       putStrLn ("Current word is: " ++ initWord)
       putStrLn ("Guesses: " ++ addSpaces (sort (allGuesses ++ [ch])) ) 
       (if gWrcnt + 1 == max
           then gameover
       else 
          hangman initWord initDict (allGuesses ++ [ch]) 
           (gWrcnt + 1) prtDictlen max)
  else if not(elem ch (head newDict))
    then do 
       putStrLn "Bad guess"
       putStrLn ("Current word is: " ++ initWord)
       putStrLn ("Guesses: " ++ addSpaces (sort (allGuesses ++ [ch])) ) 
       (if gWrcnt + 1 == max
           then gameover
       else 
          hangman initWord newDict (allGuesses ++ [ch]) 
           (gWrcnt + 1) prtDictlen max)
  else if length newDict > 1 || blanks newWord > 0
    then do 
       putStrLn "Good guess"
       putStrLn ("Current word is: " ++ newWord)
       putStrLn ("Guesses: " ++ addSpaces (sort (allGuesses ++ [ch])) ) 
       hangman newWord newDict (allGuesses ++ [ch]) gWrcnt prtDictlen max
  else if length newDict == 1 && length initDict == 1
    then do 
       putStrLn ("Current word is: " ++ newWord)
       putStrLn ("Guesses: " ++ addSpaces (sort (allGuesses ++ [ch])) ) 
       putStrLn "You Win!"
  else if (length newDict) == 1 && elem (head newDict) initDict
    then do 
     putStrLn "Bad guess"
     putStrLn ("Current word is: " ++ initWord)
     putStrLn ("Guesses: " ++ addSpaces (sort (allGuesses ++ [ch])) )
     (if gWrcnt + 1 == max
           then gameover
       else
          hangman initWord (delete (head newDict) initDict) 
          (allGuesses ++ [ch]) (gWrcnt + 1) prtDictlen max )
  else do 
     putStrLn "Bad guess"
     putStrLn ("Current word is: " ++ initWord)
     putStrLn ("Guesses: " ++ addSpaces (sort (allGuesses ++ [ch])) )
     (if gWrcnt + 1 == max
           then gameover
       else
          hangman initWord initDict
          (allGuesses ++ [ch]) (gWrcnt + 1) prtDictlen max )

--Tells user they lost
gameover = do
  putStrLn "You Lose!"

--start of Evil Hangman game, reads args
mainEvil = do
  args <- getArgs 
  if length args == 5
    then do
      [mode, inFile, wlen, numG, n] <- getArgs
      putStrLn "Welcome to Evil Hangman"
      putStrLn ("After " ++ show (numG) ++ " incorrect guesses, you lose")
      putStrLn "Each underscore is a letter in the 'secret word'"
      inFileHandle <- openFile inFile ReadMode
      inputFile <- readFile inFile
      let initDict = trimtoSize (toList inputFile) (read wlen :: Int)
      hClose inFileHandle
      let initWord = initBlanks (head initDict)
      putStrLn ("Current word is " ++ initWord)
      hangman initWord initDict "" 0 (args!!4 == "-n") (read numG :: Int)
  else do
    [mode, inFile, wlen, numG] <- getArgs
    putStrLn "Welcome to Evil Hangman"
    putStrLn ("After" ++ show (numG) ++ "incorrect guesses, you lose")
    putStrLn "Each underscore is a letter in the 'secret word'"
    inFileHandle <- openFile inFile ReadMode
    inputFile <- readFile inFile
    let initDict = trimtoSize (toList inputFile) (read wlen :: Int)
    hClose inFileHandle
    let initWord = initBlanks (head initDict)
    putStrLn ("Current word is " ++ initWord)
    hangman initWord initDict "" 0 False (read numG :: Int)