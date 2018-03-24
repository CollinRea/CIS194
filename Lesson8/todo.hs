module Todo where

import System.Environment
import System.Directory
import System.IO
import System.IO.Error
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
    Just (action) -> catchIOError (action args) handleMissingFile
    Nothing -> errorExit a


-- Action Functions

add :: [String] -> IO ()
add [fileName, todoItem] = do
  appendFile fileName (todoItem ++ "\n")
  view [fileName]

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let fileContents = lines contents
      numberedLines = zipWith (\n line -> show n ++ " - " ++ line) [0..] fileContents
  putStr $ unlines numberedLines

remove :: [String] -> IO ()
remove [fileName, num] = do  
  handle <- openFile fileName ReadMode  
  (tempName, tempHandle) <- openTempFile "." "temp"  
  contents <- hGetContents handle  
  let number = read num  
      fileContents = lines contents  
      newFileContents = delete (fileContents !! number) fileContents  
  hPutStr tempHandle $ unlines newFileContents  
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
      fileContents = lines contents  
      lineToBump = (fileContents !! number)
      newFileContents = lineToBump : delete lineToBump fileContents 
  hPutStr tempHandle $ unlines newFileContents  
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

-- Error handler
handleMissingFile :: IOError -> IO ()
handleMissingFile e
  | isDoesNotExistError e =
    case ioeGetFileName e of 
      Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path
      Nothing -> putStrLn "Whoops! File does not exist at unknown location!"
  | otherwise = ioError e
