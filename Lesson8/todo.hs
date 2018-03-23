import System.Environment
import System.Directory
import System.IO
import Data.List

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
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args

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