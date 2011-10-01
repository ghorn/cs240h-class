-- Lab1.hs

{-# OPTIONS_GHC -Wall #-}

module Main where

import System.Environment(getArgs)
import System.IO(hReady, stdin)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import Data.List(sortBy)

data WordCount = WordCount { wcText :: T.Text, wcLength :: Int, wcNum :: Int }

-- Take command line arguments or stdin and return one giant Text object
-- Throw errors if both or neither of [stdin,filenames] are provided
getRawInput :: IO T.Text
getRawInput = do
  args <- getArgs
  hasStdin <- hReady stdin
  
  -- Take a bunch of filenames and return a giant concatenated Text object
  let readFiles :: [String] -> IO T.Text
      readFiles filenames = do
        output <- mapM T.readFile filenames
        return $ T.concat output
  
  let output
        | hasStdin && length args > 0 = error "stdin and filename arguments... so confused"
        | hasStdin                    = T.hGetContents stdin
        | length args > 0             = readFiles args
        | otherwise                   = error "no stdin or filename arguments... so sad"
  output

-- Turn giant text gob into a bunch of words suitible for counting
parseHugeTextGob :: T.Text -> [T.Text]
parseHugeTextGob raw = goodWords
  where
    -- make all this text lowercase and split by anything that's not an apostrophe or ['a'..'z']
    wordsWithEmpties = T.split (flip notElem ('\'':['a'..'z'])) $ T.toLower raw
    
    -- this leaves in empty objects, e.g. "ab,c,,d" would become ["ab","c","","d"]
    wordsWithoutEmpties = filter (not . T.null) wordsWithEmpties

    -- remove all non-infix appostrophes
    -- this will not deal with multiple appostrophes like "can''t" or "i'can't"
    goodWords = map removeNonInfix' wordsWithoutEmpties
      where
        removeNonInfix' :: T.Text -> T.Text
        removeNonInfix' txt
          | T.isPrefixOf (T.pack "'") txt = removeNonInfix' (T.tail txt)
          | T.isSuffixOf (T.pack "'") txt = removeNonInfix' (T.init txt)
          | otherwise                     = txt
  
-- count the words
countWords :: [T.Text] -> [WordCount]
countWords wordsList = map toWc sortedCount
  where
    toWc :: (T.Text, Int) -> WordCount
    toWc (txt, n) = WordCount {wcText = txt, wcLength = T.length txt, wcNum = n}
    
    sortedCount :: [(T.Text, Int)]
    sortedCount = sortBy (\(_,a) (_,b) -> compare b a) unsortedCount

    unsortedCount :: [(T.Text, Int)]
    unsortedCount = M.toList (foldl insert' M.empty wordsList)
      where
        -- if the word is not already in the map, insert' will put it in with a value of 1
        -- if the words is already in the map, insert' will increment it's value
        insert' theMap newWord = M.insertWith (+) newWord 1 theMap
    

-- draw ascii histogram
printHistogram :: [WordCount] -> IO ()
printHistogram wordCounts = do
  let maxWordLength = maximum $ map wcLength wordCounts
      maxNumInstances = wcNum $ head wordCounts
      charsForHistogram = eightyCharsExceeded `seq` 80 - maxWordLength - 1
      eightyCharsExceeded
        | maxWordLength <= 78 = ()
        | otherwise           = error $ "you've got a long ass word in there, "
                                ++ "i can't print a histogram in less than 80 colums"
      
      printHistogramEntry :: WordCount -> IO ()
      printHistogramEntry wc = do 
        let
          spaces = replicate (maxWordLength - (wcLength wc) + 1) ' '
          scaledInt = ((wcNum wc) * charsForHistogram) `div` maxNumInstances
          bar = take scaledInt (cycle "#")
        if length bar > 0
          then do T.putStr (wcText wc) 
                  putStrLn $ spaces ++ bar
          else return ()

  mapM_ printHistogramEntry wordCounts

main :: IO ()
main = do
  input <- getRawInput
  printHistogram $ countWords $ parseHugeTextGob input
