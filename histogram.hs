
-- so far, I'm just trying to get a simple word count function going,
-- the output should be 2-tuples with a word and how many times that 
-- word appears in a given string. There is no file input or output yet.

import Data.List
import Data.Char

getFreq :: String -> [(String, Int)]

getFreq = map (\str -> (head str, length str)) . group . sort . map (stripWord) . words



-- now, to cut out the punctuation at the beginning and end of words
-- (I'm going to assume beginning and end punctuation are the only non
-- word punctuation

stripWord :: String -> String

stripWord word 
    | (isPunctuation $ head word) = stripWord $ tail word
    | (isPunctuation $ last word) = stripWord $ init word
    | otherwise                   = word
