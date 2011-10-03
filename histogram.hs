

import Data.List
import Data.Char
import Control.Monad
import System.IO
import System

--takes a string and outputs a list of tuples that contains the word and the number of appearances

getFreq :: String -> [(String, Int)]
getFreq = sortBy sortFreq . map (\str -> (head str, length str)) . group . sort . filter notNull . map (stripWord) . words . map toLower

-- convenient notNull fun

notNull :: [a] -> Bool
notNull [] = False
notNull _  = True

-- function that orders tuples by number of occurences then alphabetically

sortFreq :: (String, Int) -> (String, Int) -> Ordering
sortFreq (str1, int1) (str2, int2)
    | int1 > int2 = LT
    | int1 < int2 = GT
    | int1 ==int2 = compare str1 str2

-- function that finds the largest word length in the entire list

longestWord = length . fst . maximumBy wordLength

-- an ordering by the longest word

wordLength :: (String, Int) -> (String, Int) -> Ordering
wordLength (str1, _) (str2, _) = compare (length str1) (length str2)

-- now, to cut out the punctuation at the beginning and end of words
-- (I'm going to assume beginning and end punctuation are the only non
-- word punctuation)

stripWord :: String -> String
stripWord [] = []
stripWord word 
    | (isPunctuation $ head word) = stripWord $ tail word
    | (isPunctuation $ last word) = stripWord $ init word
    | otherwise                   = word
    
-- gets the number of asterisks needed for the histogram

numAst :: Int -> Int -> Int -> Int
numAst biggest current wordSize = round $ fromIntegral current / fromIntegral biggest * fromIntegral (80-wordSize)

--prints a line of the histogram
printHist :: Int -> Int -> (String, Int) -> IO ()
printHist wordlen ast word = do
    when (ast > 0) $ putStrLn $  (fst word) ++ (replicate (wordlen - (length $ fst word)) ' ') ++ (replicate ast '*')

--prints all of the lines
printList :: [(String, Int)] -> Int -> Int -> IO ()
printList (xs:list) freq len = do
    printHist len (numAst freq (snd xs) len) xs
    when (list /= []) $ printList list freq len
 
--reads data from either the terminal or a file specified in the terminal

getInp :: [String] -> IO String
getInp [] = getContents
getInp args = getFileStrings [] args
    where getFileStrings string [] = return string
          getFileStrings string (next:remaining) = do
                curFile <- openFile next ReadMode
                contents <- hGetContents curFile
                getFileStrings (contents ++ " " ++ string) remaining

main = do
    args <- getArgs
    contents <- getInp args
    let list = getFreq contents
    let maxfreq = snd $ head list
    let maxlen = longestWord list + 1 
    printList list maxfreq maxlen

