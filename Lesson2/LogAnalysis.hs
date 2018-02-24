{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1

-- Parse individual String messages
parseMessage :: String -> LogMessage
parseMessage s
  | x == "I" = LogMessage Info fstInt (unwords xs)
  | x == "W" = LogMessage Warning fstInt (unwords xs)
  | x == "E" = LogMessage (Error fstInt) (read (head xs) :: Int) (unwords $ tail xs)
  | otherwise = Unknown s
  where 
        (x:y:xs) = words s
        fstInt = read y :: Int

-- Parse a multi-line string and return list of LogMessage(s)
parse :: String -> [LogMessage]
parse "" = []
parse logFile = map parseMessage (lines logFile)


-- Exercise 2

-- Insert a Log Message into a Binary Message Tree
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert m Leaf = Node Leaf m Leaf
insert m (Node left (Unknown _) right) = Node left m right
insert m@(LogMessage _ x _) bt@(Node left n@(LogMessage _ y _) right)
    | x < y     = Node (insert m left) n right
    | x > y     = Node left n (insert m right)
    | otherwise = bt


-- Exercise 3

-- Build Message Tree from List of Log Messages
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (m:ms) = (insert m (build ms))

-- Test function to read a file and build a MessageTree
readAndParseAndBuild :: FilePath -> IO [LogMessage]
readAndParseAndBuild fileName = do
  logs <- readFile fileName
  return $ inOrder $ build $ parse logs


-- Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf m Leaf) = [m]
inOrder (Node left m right) =
  (inOrder left) ++ [m] ++ (inOrder right)


-- Exercise 5

-- Get Errors over 50 severity and return List of those Strings in order
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong ls =
  filter (\x -> x /= "") $ map findErrors (inOrder $ build ls)
  where 
    findErrors (LogMessage (Error n) _ s)
      | n > 50 = s
    findErrors _ = ""
