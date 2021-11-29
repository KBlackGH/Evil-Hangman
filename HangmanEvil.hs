--Kalil Black

module Main where
import System.Environment
import System.IO
import Data.List
import Data.Char(toUpper)

--Find all words of given length
--Find all words containing ch (if 0 bad guess)
--charAppear perms 1 to len for all permute ch+(len-chnum _)
--remove duplicates from permutes
--go through all words looking for each perm then list in list
--find largest length (newDictionary)

toList :: String -> [String]
toList input = (words input)

trimtoSize :: [String] -> Int -> [String]
trimtoSize slist len
  |null slist = slist
  |length (head slist) == len = [map toUpper (head slist)] ++
   trimtoSize (tail slist) len
  |otherwise = trimtoSize (tail slist) len

newLists :: String -> [String] -> [String] -> Char -> [[String]]
newLists gw perms currDict c
  |null perms = [[]]
  |otherwise = [nLHelper gw (head perms) currDict c] ++ 
   newLists gw (tail perms) currDict c

nLHelper :: String -> String -> [String] -> Char -> [String]
nLHelper gw perms currDict c
  |null currDict = []
  |isPermMatch perms (head currDict) gw c = [head currDict] ++
   nLHelper gw perms (tail currDict) c
  |otherwise = nLHelper gw perms (tail currDict) c

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

updateDict :: Char -> String -> [String] -> [String]
updateDict ch gw currDict
    |null (containCh ch currDict) = []
    |otherwise = upHelp fullList []
    where
        fullList = newLists gw (charPerms ch (blanks gw)
         (blanks gw)) (containCh ch currDict) ch

upHelp :: [[String]] -> [String] -> [String]
upHelp slists largest
  |null slists = largest
  |length (head slists) > length largest = upHelp (tail slists) (head slists)
  |otherwise = upHelp (tail slists) largest

blanks :: String -> Int
blanks s
  |null s = 0
  |head s == '_' = 1 + blanks (tail (tail s))
  |otherwise = blanks (tail (tail s))

containCh :: Char -> [String] -> [String]
containCh ch sl
  |null sl = sl
  |elem ch (head sl) = [head sl] ++ containCh ch (tail sl)
  |otherwise = containCh ch (tail sl)

charPerms :: Char -> Int -> Int -> [String]
charPerms ch currlen tlen
  |currlen < 1 = []
  |otherwise = nub (permutations (initperm ch currlen tlen)) ++ 
   charPerms ch (currlen - 1) tlen

initperm :: Char -> Int -> Int -> String
initperm ch currlen tlen
  |tlen < 1 = ""
  |currlen < 1 = "_" ++ initperm ch currlen (tlen - 1)
  |otherwise = [ch] ++ initperm ch (currlen - 1) (tlen - 1)

initBlanks :: String -> String
initBlanks "" = ""
initBlanks s = "_ " ++ initBlanks (tail s)

updateBlanks :: Char-> String -> String -> String
updateBlanks c s1 s2
  |null s2 = ""
  |not(elem (toUpper c) s2) = s1
  |otherwise = updtHelper (toUpper c) s1 s2

updtHelper :: Char-> String -> String -> String
updtHelper c s1 s2
  |null s1 = s1
  |head s2 == c = [c] ++ " " ++ updtHelper c (tail (tail s1)) (tail s2)
  |head s1 == '_' = "_ " ++ 
   updtHelper c (tail (tail s1)) (tail s2)
  |otherwise = [head s1] ++ [head (tail s1)] ++ 
   updtHelper c (tail (tail s1)) (tail s2)

addSpaces :: String -> String
addSpaces "" = ""
addSpaces s1 = [head s1] ++ " " ++ addSpaces (tail s1)

--if null upD badguess
--if updD > 1 good guess
--if both equal 1, Win
--If length currDict >1 && length updatedDict ==1 
-Bad guess, currdict has udatedDict word removed

hangman initWord initDict allGuesses gWrcnt prtDictlen max = do
  if prtDictlen
    then putStrLn ("Current Dictionary Size: " ++ show (length initDict) ++ show (initDict))
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

gameover = do
  putStrLn "You Lose!"

main = do
  args <- getArgs 
  if length args == 4
    then do
      [inFile, wlen, numG, n] <- getArgs
      putStrLn "Welcome to Evil Hangman"
      putStrLn ("After " ++ show (numG) ++ " incorrect guesses, you lose")
      putStrLn "Each underscore is a letter in the 'secret word'"
      inFileHandle <- openFile inFile ReadMode
      inputFile <- readFile inFile
      let initDict = trimtoSize (toList inputFile) (read wlen :: Int)
      hClose inFileHandle
      let initWord = initBlanks (head initDict)
      putStrLn ("Current word is " ++ initWord)
      hangman initWord initDict "" 0 (args!!3 == "-n") (read numG :: Int)
  else do
    [inFile, wlen, numG] <- getArgs
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
  
{-
  if length args == 4
    then hangman initWord initDict "" 0 True numG
    else hangman initWord initDict "" 0 False numG
  [inFile, wlen, numG, n] <- getArgs
  putStrLn "Welcome to Hangman"
  putStrLn ("After " ++ show (numG) ++ " incorrect guesses, you lose")
  putStrLn "Each underscore is a letter in the 'secret word'"
  inFileHandle <- openFile inFile ReadMode
  inputFile <- readFile inFile
  let initDict = trimtoSize (toList inputFile) (read wlen :: Int)
  hClose inFileHandle
  let initWord = initBlanks (head initDict)
  putStrLn ("Current word is " ++ initWord) -}