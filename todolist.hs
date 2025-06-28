-- Haskell To-Do List CLI Application

{-# LANGUAGE DeriveGeneric #-}

import System.IO
import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Data.List (sortBy, find)
import Data.Maybe (fromJust)
import Control.Exception (catch, IOException)
import Control.DeepSeq (deepseq)
import Data.Char (isAlphaNum)
import GHC.Generics (Generic)

-- Data Types

data Priority = Low | Medium | High deriving (Show, Read, Eq, Ord, Generic)

data Task = Task
  { title    :: String
  , category :: String
  , dueDate  :: Maybe Day
  , priority :: Priority
  } deriving (Show, Read, Generic)

type TaskList = [Task]

-- Task Operations

addTask :: TaskList -> Task -> TaskList
addTask tasks task = tasks ++ [task]

removeTask :: TaskList -> String -> TaskList
removeTask tasks taskTitle = filter ((/= taskTitle) . title) tasks

editTask :: TaskList -> String -> Task -> TaskList
editTask tasks taskTitle newTask = map (\t -> if title t == taskTitle then newTask else t) tasks

findTask :: TaskList -> String -> Maybe Task
findTask tasks taskTitle = find ((== taskTitle) . title) tasks

-- Task Display

comparePriority :: Task -> Task -> Ordering
comparePriority t1 t2 = compare (priority t2) (priority t1)

listTasks :: TaskList -> IO ()
listTasks tasks = mapM_ print (sortBy comparePriority tasks)

-- File Operations

saveTasks :: FilePath -> TaskList -> IO ()
saveTasks filePath tasks = withFile filePath WriteMode $ \handle ->
  hPutStr handle (show tasks)

loadTasks :: FilePath -> IO TaskList
loadTasks filePath = catch (withFile filePath ReadMode $ \handle -> do
  content <- hGetContents handle
  content `deepseq` return (read content :: TaskList)) handleReadError
  where
    handleReadError :: IOException -> IO TaskList
    handleReadError _ = return []

loadTasksStrict :: FilePath -> IO String
loadTasksStrict filePath = catch (withFile filePath ReadMode $ \handle -> do
  content <- hGetContents handle
  content `deepseq` return content) handleReadError
  where
    handleReadError :: IOException -> IO String
    handleReadError _ = return "Error reading the file."

-- Main CLI Loop

main :: IO ()
main = do
  let filePath = "tasks.txt"
  putStrLn "Welcome to the Task Management System (To-Do List)"
  tasks <- loadTasks filePath
  commandLoop tasks filePath

commandLoop :: TaskList -> FilePath -> IO ()
commandLoop tasks filePath = do
  putStrLn "\nCommands:"
  putStrLn "1. Add Task"
  putStrLn "2. Remove Task"
  putStrLn "3. Edit Task"
  putStrLn "4. List Tasks (Lazy Evaluation)"
  putStrLn "5. Show File Content (Strict Evaluation)"
  putStrLn "6. Exit"
  putStr "Enter a command: "
  hFlush stdout
  command <- getLine
  case command of
    "1" -> do
      newTask <- promptNewTask
      let updatedTasks = addTask tasks newTask
      saveTasks filePath updatedTasks
      commandLoop updatedTasks filePath
    "2" -> do
      putStr "Enter the title of the task to remove: "
      hFlush stdout
      taskTitle <- getLine
      let updatedTasks = removeTask tasks taskTitle
      saveTasks filePath updatedTasks
      commandLoop updatedTasks filePath
    "3" -> do
      putStr "Enter the title of the task to edit: "
      hFlush stdout
      taskTitle <- getLine
      case findTask tasks taskTitle of
        Just oldTask -> do
          putStrLn "Enter the new details of the task (leave blank to keep the current value)"
          newTask <- promptEditTask oldTask
          let updatedTasks = editTask tasks taskTitle newTask
          saveTasks filePath updatedTasks
          commandLoop updatedTasks filePath
        Nothing -> do
          putStrLn "Task not found."
          commandLoop tasks filePath
    "4" -> do
      putStrLn "\nTask List (Lazy Evaluation):"
      listTasks tasks
      commandLoop tasks filePath
    "5" -> do
      putStrLn "\nFile Content (Strict Evaluation):"
      content <- loadTasksStrict filePath
      putStrLn content
      commandLoop tasks filePath
    "6" -> putStrLn "Exiting..."
    _   -> do
      putStrLn "Unrecognized command."
      commandLoop tasks filePath

-- User Prompts

promptNewTask :: IO Task
promptNewTask = do
  t <- promptValidated "Title: "
  c <- promptValidated "Category: "
  putStr "Due date (YYYY-MM-DD, optional): "
  hFlush stdout
  dStr <- getLine
  let d = if null dStr then Nothing else parseDate dStr
  p <- promptPriority
  return (Task t c d p)

promptEditTask :: Task -> IO Task
promptEditTask (Task t c d p) = do
  newTitle <- promptWithDefault "Title: " t
  newCategory <- promptWithDefault "Category: " c
  putStr "Due date (YYYY-MM-DD, optional): "
  hFlush stdout
  dStr <- getLine
  let finalDueDate = if null dStr then d else parseDate dStr
  putStr "Priority (low, medium, high): "
  hFlush stdout
  pStr <- getLine
  let finalPriority = if null pStr then p else fromJust (stringToPriority pStr)
  return (Task newTitle newCategory finalDueDate finalPriority)

-- Input Validation

isValidInput :: String -> Bool
isValidInput input = all (`notElem` invalidChars) input
  where
    invalidChars = "#$%&/()=\"'"

promptValidated :: String -> IO String
promptValidated prompt = do
  putStr prompt
  hFlush stdout
  input <- getLine
  if null input || not (isValidInput input)
    then do
      putStrLn "Invalid input. It must not be empty and must not contain prohibited characters."
      promptValidated prompt
    else return input

promptWithDefault :: String -> String -> IO String
promptWithDefault prompt def = do
  putStr prompt
  hFlush stdout
  input <- getLine
  if null input
    then return def
    else if not (isValidInput input)
      then do
        putStrLn "Invalid input. Must not contain prohibited characters."
        promptWithDefault prompt def
      else return input

promptPriority :: IO Priority
promptPriority = do
  putStr "Priority (low, medium, high): "
  hFlush stdout
  pStr <- getLine
  case stringToPriority pStr of
    Just p -> return p
    Nothing -> do
      putStrLn "Unrecognized priority. Please try again."
      promptPriority

stringToPriority :: String -> Maybe Priority
stringToPriority "low"    = Just Low
stringToPriority "medium" = Just Medium
stringToPriority "high"   = Just High
stringToPriority _        = Nothing

parseDate :: String -> Maybe Day
parseDate dateStr = parseTimeM True defaultTimeLocale "%Y-%m-%d" dateStr
