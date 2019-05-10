
{-# LANGUAGE OverloadedStrings #-}

module Model
where
import Data.Text
import Data.Int
import Data.Time
import Database.SQLite.Simple

import System.Directory  -- doesFileExist
import System.IO.Error  -- mkIOError, ...

-- ---------------------------------------------------------------
-- Model: Types

type UserId = Text

type ThemeId = Int64

data Theme = Theme
        { tLeader :: UserId
        , tCategory :: Text
        , tTitle :: Text
        , tDescription :: Text
        }
        deriving (Show)

type QuestionId = Int64

data Question = Question
        { qTheme :: ThemeId
        , qUser :: UserId
        , qPosted :: UTCTime
        , qTitle :: Text
        , qText :: Text
        }
        deriving (Show)

type AnswerId = Int64

data Answer = Answer
        { aQuestion :: QuestionId
        , aUser :: UserId
        , aPosted :: UTCTime
        , aText :: Text
        }
        deriving (Show)

-- ---------------------------------------------------------------
-- Model: Data base connection

type ForumDb = Connection

openExistingDb :: Text -> IO ForumDb
openExistingDb path = do
    let path' = unpack path
    ok <- doesFileExist path'
    if ok then open path'
          else ioError $ mkIOError doesNotExistErrorType "Cannot open data base file" Nothing (Just path')

openDb :: Text -> IO ForumDb
openDb path = do
    conn <- open $ unpack path
    execute_ conn "CREATE TABLE IF NOT EXISTS themes (id INTEGER PRIMARY KEY, leader TEXT, category TEXT, title TEXT, description TEXT);\n\
                  \CREATE TABLE IF NOT EXISTS questions (id INTEGER PRIMARY KEY, theme INTEGER, user TEXT, posted DATE, title TEXT, body TEXT);\n\
                  \CREATE TABLE IF NOT EXISTS answers (id INTEGER PRIMARY KEY, question INTEGER, user TEXT, posted DATE, body TEXT);"
    pure conn

closeDb :: ForumDb -> IO ()
closeDb conn =
    close conn

-- ---------------------------------------------------------------
-- Model: Data base access: Theme

instance FromRow Theme where
  fromRow = Theme <$> field <*> field <*> field <*> field

instance ToRow Theme where
  toRow (Theme leader cat title desc) = toRow (leader, cat, title, desc)

getTheme :: ThemeId -> ForumDb -> IO (Maybe Theme)
getTheme tid conn = do
        rows <- query conn "SELECT leader,category,title,description FROM themes WHERE id = ?" (Only tid)
        case rows of
            [] -> pure Nothing
            [row] -> pure $ Just row

getThemeList :: ForumDb -> IO [(ThemeId, Theme)]
getThemeList conn = do
        rows <- query_ conn "SELECT * FROM themes"
        pure $ unrow <$> rows
    where unrow (Only tid :. entity) = (tid, entity)

addTheme :: Theme -> ForumDb -> IO ThemeId
addTheme theme conn = do
    execute conn "INSERT INTO themes (leader,category,title,description) VALUES (?,?,?,?)" theme
    lastInsertRowId conn

updateTheme :: ThemeId -> Theme -> ForumDb -> IO ()
updateTheme tid theme conn =
    execute conn "UPDATE themes SET leader=?,category=?,title=?,description=? WHERE id = ?" (theme :. Only tid)

deleteTheme :: ThemeId -> ForumDb -> IO ()
deleteTheme tid conn =
    execute conn "DELETE FROM themes WHERE id = ?" (Only tid)

-- ---------------------------------------------------------------
-- Model: Data base access: Question

instance FromRow Question where
  fromRow = Question <$> field <*> field <*> field <*> field <*> field

instance ToRow Question where
  toRow (Question tid user posted title body) = toRow (tid, user, posted, title, body)

getQuestion :: QuestionId -> ForumDb -> IO (Maybe Question)
getQuestion qid conn = do
        rows <- query conn "SELECT theme,user,posted,title,body FROM questions WHERE id = ?" (Only qid)
        case rows of
            [] -> pure Nothing
            [row] -> pure $ Just row

getQuestionList :: ThemeId -> ForumDb -> IO [(QuestionId, Question)]
getQuestionList tid conn = do
        rows <- query conn "SELECT * FROM questions WHERE theme = ?" (Only tid)
        pure $ unrow <$> rows
    where unrow (Only qid :. entity) = (qid, entity)

addQuestion :: Question -> ForumDb -> IO QuestionId
addQuestion theme conn = do
    execute conn "INSERT INTO questions (theme,user,posted,title,body) VALUES (?,?,?,?,?)" theme
    lastInsertRowId conn

updateQuestion :: QuestionId -> Question -> ForumDb -> IO ()
updateQuestion qid theme conn =
    execute conn "UPDATE questions SET theme=?,user=?,posted=?,title=?,body=? WHERE id = ?" (theme :. Only qid)

deleteQuestion :: QuestionId -> ForumDb -> IO ()
deleteQuestion qid conn =
    execute conn "DELETE FROM questions WHERE id = ?" (Only qid)

-- ---------------------------------------------------------------
-- Model: Data base access: Answer

instance FromRow Answer where
  fromRow = Answer <$> field <*> field <*> field <*> field

instance ToRow Answer where
  toRow (Answer qid user posted body) = toRow (qid, user, posted, body)

getAnswer :: AnswerId -> ForumDb -> IO (Maybe Answer)
getAnswer aid conn = do
        rows <- query conn "SELECT question,user,posted,body FROM answers WHERE id = ?" (Only aid)
        case rows of
            [] -> pure Nothing
            [row] -> pure $ Just row

getAnswerList :: QuestionId -> ForumDb -> IO [(AnswerId, Answer)]
getAnswerList qid conn = do
        rows <- query conn "SELECT * FROM answers WHERE question = ?" (Only qid)
        pure $ unrow <$> rows
    where unrow (Only aid :. entity) = (aid, entity)

addAnswer :: Answer -> ForumDb -> IO AnswerId
addAnswer theme conn = do
    execute conn "INSERT INTO answers (question,user,posted,body) VALUES (?,?,?,?)" theme
    lastInsertRowId conn

updateAnswer :: AnswerId -> Answer -> ForumDb -> IO ()
updateAnswer aid theme conn =
    execute conn "UPDATE answers SET question=?,user=?,posted=?,body=? WHERE id = ?" (theme :. Only aid)

deleteAnswer :: AnswerId -> ForumDb -> IO ()
deleteAnswer aid conn =
    execute conn "DELETE FROM answers WHERE id = ?" (Only aid)

