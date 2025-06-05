import System.IO
import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Data.List (sortBy, find)
import Data.Maybe (fromJust)
import Control.Exception (catch, IOException)
import Control.DeepSeq (deepseq)
import Data.Char (isAlphaNum)

data Priority = Low | Medium | High deriving (Show, Read, Eq, Ord)
data Task = Task { title :: String
                 , category :: String
                 , dueDate :: Maybe Day
                 , priority :: Priority
                 } deriving (Show, Read)

type TaskList = [Task]

addTask :: TaskList -> Task -> TaskList
addTask tasks task = tasks ++ [task]

removeTask :: TaskList -> String -> TaskList
removeTask tasks taskTitle = filter ((/= taskTitle) . title) tasks

editTask :: TaskList -> String -> Task -> TaskList
editTask tasks taskTitle newTask = map (\t -> if title t == taskTitle then newTask else t) tasks

comparePriority :: Task -> Task -> Ordering
comparePriority t1 t2 = compare (priority t2) (priority t1)

listTasks :: TaskList -> IO ()
listTasks tasks = mapM_ print (sortBy comparePriority tasks)

saveTasks :: FilePath -> TaskList -> IO ()
saveTasks filePath tasks = withFile filePath WriteMode $ \handle -> do
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
    handleReadError _ = return "Error al leer el archivo."

main :: IO ()
main = do
    let filePath = "tasks.txt"
    putStrLn "Bienvenido al Sistema de Gestión de Tareas (To-Do List)"
    tasks <- loadTasks filePath
    commandLoop tasks filePath

commandLoop :: TaskList -> FilePath -> IO ()
commandLoop tasks filePath = do
    putStrLn "\nComandos: "
    putStrLn "1. Agregar tarea"
    putStrLn "2. Eliminar tarea"
    putStrLn "3. Editar tarea"
    putStrLn "4. Listar tareas (Evaluación perezosa)"
    putStrLn "5. Mostrar contenido del archivo (Sin evaluación perezosa)"
    putStrLn "6. Salir"
    putStr "Ingrese un comando: "
    hFlush stdout
    command <- getLine
    case command of
        "1" -> do
            newTask <- promptNewTask
            let updatedTasks = addTask tasks newTask
            saveTasks filePath updatedTasks
            commandLoop updatedTasks filePath
        "2" -> do
            putStr "Ingrese el título de la tarea a eliminar: "
            hFlush stdout
            taskTitle <- getLine
            let updatedTasks = removeTask tasks taskTitle
            saveTasks filePath updatedTasks
            commandLoop updatedTasks filePath
        "3" -> do
            putStr "Ingrese el título de la tarea a editar: "
            hFlush stdout
            taskTitle <- getLine
            case findTask tasks taskTitle of
                Just oldTask -> do
                    putStrLn "Ingrese los nuevos detalles de la tarea (deje en blanco para mantener el valor original)"
                    newTask <- promptEditTask oldTask
                    let updatedTasks = editTask tasks taskTitle newTask
                    saveTasks filePath updatedTasks
                    commandLoop updatedTasks filePath
                Nothing -> do
                    putStrLn "Tarea no encontrada."
                    commandLoop tasks filePath
        "4" -> do
            putStrLn "\nListado de tareas (Evaluación perezosa):"
            listTasks tasks
            commandLoop tasks filePath
        "5" -> do
            putStrLn "\nContenido del archivo (Sin evaluación perezosa):"
            content <- loadTasksStrict filePath
            putStrLn content
            commandLoop tasks filePath
        "6" -> putStrLn "Saliendo..."
        _   -> do
            putStrLn "Comando no reconocido."
            commandLoop tasks filePath

findTask :: TaskList -> String -> Maybe Task
findTask tasks taskTitle = find ((== taskTitle) . title) tasks

promptNewTask :: IO Task
promptNewTask = do
    t <- promptValidated "Título: "
    c <- promptValidated "Categoría: "
    putStr "Fecha de vencimiento (YYYY-MM-DD, opcional): "
    hFlush stdout
    dStr <- getLine
    let d = if null dStr then Nothing else parseDate dStr
    p <- promptPriority
    return (Task t c d p)

promptEditTask :: Task -> IO Task
promptEditTask (Task t c d p) = do
    newTitle <- promptWithDefault "Título: " t
    newCategory <- promptWithDefault "Categoría: " c
    putStr "Fecha de vencimiento (YYYY-MM-DD, opcional): "
    hFlush stdout
    dStr <- getLine
    let finalDueDate = if null dStr then d else parseDate dStr
    putStr "Prioridad (baja, media, alta): "
    hFlush stdout
    pStr <- getLine
    let finalPriority = if null pStr then p else fromJust (spanishToPriority pStr)
    return (Task newTitle newCategory finalDueDate finalPriority)

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
            putStrLn "Entrada inválida. No debe estar vacía y no debe contener caracteres no permitidos."
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
                putStrLn "Entrada inválida. No debe contener caracteres no permitidos."
                promptWithDefault prompt def
            else return input

promptPriority :: IO Priority
promptPriority = do
    putStr "Prioridad (baja, media, alta): "
    hFlush stdout
    pStr <- getLine
    let maybePriority = spanishToPriority pStr
    case maybePriority of
        Just p -> return p
        Nothing -> do
            putStrLn "Prioridad no reconocida. Intente nuevamente."
            promptPriority

spanishToPriority :: String -> Maybe Priority
spanishToPriority "baja"  = Just Low
spanishToPriority "media" = Just Medium
spanishToPriority "alta"  = Just High
spanishToPriority _       = Nothing

parseDate :: String -> Maybe Day
parseDate dateStr = parseTimeM True defaultTimeLocale "%Y-%m-%d" dateStr