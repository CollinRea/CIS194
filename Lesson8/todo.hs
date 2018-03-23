{-# OPTIONS_GHC -Wall #-}
module Todo where

import System.Environment
import System.Directory
import System.IO
import Data.List
import Data.Time.Clock

-- List of actions that can be taken as first Command line argument
dispatch :: [(String, [String] -> IO ())]
dispatch = 
  [ ("add", add)
  , ("view", view)
  , ("remove", remove)
  , ("bump", bump)
  ]

-- Interact with the world
main :: IO ()
main = do
  a@(command:args) <- getArgs
  let input = lookup command dispatch
  case input of
    Just (action) -> action args
    Nothing -> errorExit a


-- Action Functions

add :: [String] -> IO ()
add [fileName, todoItem] = do
  appendFile fileName (todoItem ++ "\n")
  view [fileName]

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
  putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, num] = do  
  handle <- openFile fileName ReadMode  
  (tempName, tempHandle) <- openTempFile "." "temp"  
  contents <- hGetContents handle  
  let number = read num  
      todoTasks = lines contents  
      newTodoItems = delete (todoTasks !! number) todoTasks  
  hPutStr tempHandle $ unlines newTodoItems  
  hClose handle  
  hClose tempHandle  
  removeFile fileName  
  renameFile tempName fileName
  view [fileName]

bump :: [String] -> IO ()
bump [fileName, num] = do 
  handle <- openFile fileName ReadMode  
  (tempName, tempHandle) <- openTempFile "." "temp"  
  contents <- hGetContents handle  
  let number = read num  
      todoTasks = lines contents  
      todoToBump = (todoTasks !! number)
      newTodoItems = todoToBump : delete todoToBump todoTasks 
  hPutStr tempHandle $ unlines newTodoItems  
  hClose handle  
  hClose tempHandle  
  removeFile fileName  
  renameFile tempName fileName
  view [fileName]

-- Write to error.txt log and display message to user
errorExit :: [String] -> IO ()
errorExit args = do
  time <- getCurrentTime
  errorArg <- (pure . unwords) args
  appendFile "errors.txt" (show time ++ " - " ++ errorArg ++ "\n")
  putStr ("Invalid input: '" ++ errorArg ++ "'")