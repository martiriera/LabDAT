
{-# LANGUAGE OverloadedStrings #-}

module Model
where
import Data.Text
import Data.Int
import Database.SQLite.Simple

import System.Directory  -- doesFileExist
import System.IO.Error  -- mkIOError, ...

-- ****************************************************************
-- Model

-- NOTA: Veieu en <https://en.wikibooks.org/wiki/Haskell/More_on_datatypes#Named_Fields_(Record_Syntax)>
-- una breu introducció a la sintaxis dels registres Haskell

data Task = Task { taskTitle :: Text, taskDone :: Bool }
        deriving (Show)

taskSetDone :: Bool -> Task -> Task
taskSetDone done task = task{ taskDone = done }

type TaskId = Int64

type TaskDb = Connection

instance FromRow Task where
  fromRow = Task <$> field <*> field

instance ToRow Task where
  toRow (Task title done) = toRow (title, done)

openExistingDb :: Text -> IO TaskDb
openExistingDb path = do
    let path' = unpack path
    ok <- doesFileExist path'
    if ok then open path'
          else ioError $ mkIOError doesNotExistErrorType "Cannot open data base file" Nothing (Just path')

openDb :: Text -> IO TaskDb
openDb path = do
    conn <- open $ unpack path
    execute_ conn "CREATE TABLE IF NOT EXISTS tasks (id INTEGER PRIMARY KEY, title TEXT, done INTEGER)"
    pure conn

closeDb :: TaskDb -> IO ()
closeDb conn =
    close conn

addInitDb :: TaskDb -> IO ()
addInitDb conn = do
    addTask (Task "Tasca 1" False) conn
    tid2 <- addTask (Task "Tasca 2" False) conn
    markTask tid2 conn

getTaskList :: TaskDb -> IO [(TaskId, Task)]
getTaskList conn = do
        rows <- query_ conn "SELECT * from tasks"
        pure $ unrow <$> rows
    where unrow (Only tid :. task) = (tid, task)

addTask :: Task -> TaskDb -> IO TaskId
addTask task conn = do
    execute conn "INSERT INTO tasks (title,done) VALUES (?,?)" task
    lastInsertRowId conn

markTask :: TaskId -> TaskDb -> IO ()
markTask tid conn =
    execute conn "UPDATE tasks SET done = 1 WHERE id = ?" (Only tid)

deleteTask :: TaskId -> TaskDb -> IO ()
deleteTask tid conn =
    execute conn "DELETE FROM tasks WHERE id = ?" (Only tid)

