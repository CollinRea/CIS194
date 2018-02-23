{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s
  | x == "I" = LogMessage Info fstInt (unwords xs)
  | x == "W" = LogMessage Warning fstInt (unwords xs)
  | x == "E" = LogMessage (Error fstInt) (read (head xs) :: Int) (unwords $ tail xs)
  | otherwise = Unknown s
  where 
        (x:y:xs) = words s
        fstInt = read y :: Int

parse :: String -> [LogMessage]
parse "" = []
parse logFile = map parseMessage (lines logFile)