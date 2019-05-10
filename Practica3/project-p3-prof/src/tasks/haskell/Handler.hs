
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler
where
import Found
import Model

import Develop.DatFw
import Develop.DatFw.Template
import Develop.DatFw.Auth
import Develop.DatFw.Form
import Develop.DatFw.Form.Fields

import           Control.Monad            -- imports forM_, ...
import           Control.Monad.IO.Class   -- imports liftIO
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Maybe               -- imports isJust, isNothing, catMaybes, ...
import           Data.Bool                -- imports bool
import           Text.Read (readMaybe)

-- ****************************************************************
-- Handlers del controlador de Tasks.
--      L'autenticacio d'usuaris es realitza en un subsistema separat Auth.

taskForm :: AForm (HandlerFor site) Task
taskForm =
    Task <$> freq (check (\ t -> if T.length t < 5 then Left "Ha de tenir més de 5 caràcters" else Right t) textField)
                  (withPlaceholder "Introduiu el títol de la nova tasca" "Títol")
                  Nothing
         <*> pure False

getHomeR :: HandlerFor Tasks Html
getHomeR = do
    -- Get model info
    db <- getsSite tasksDb
    tasks <- liftIO $ getTaskList db
    mbuser <- maybeAuthId
    tformw <- generateAFormPost taskForm
    -- Return HTML content
    defaultLayout $(widgetTemplFile "src/tasks/templates/home.html")

postHomeR :: HandlerFor Tasks Html
postHomeR = do
    user <- requireAuthId
    (tformr, tformw) <- runAFormPost taskForm
    db <- getsSite tasksDb
    addButton <- isJust <$> lookupPostParam "add"
    markButton <- isJust <$> lookupPostParam "mark"
    deleteButton <- isJust <$> lookupPostParam "delete"
    if addButton
      then do
        case tformr of
            FormSuccess newtask -> do
                liftIO $ addTask newtask db
                redirectRoute HomeR []
            _ -> do
                tasks <- liftIO $ getTaskList db
                let mbuser = Just user
                defaultLayout $(widgetTemplFile "src/tasks/templates/home.html")
      else if markButton then do
        checkBoxes <- lookupPostParams "tid"
        let tids = catMaybes ((readMaybe . T.unpack) <$> checkBoxes)
        forM_ tids $ \ tid ->
            liftIO $ markTask tid db
        redirectRoute HomeR []
      else if deleteButton then do
        checkBoxes <- lookupPostParams "tid"
        let tids = catMaybes ((readMaybe . T.unpack) <$> checkBoxes)
        forM_ tids $ \ tid ->
            liftIO $ deleteTask tid db
        redirectRoute HomeR []
      else
        invalidArgs ["add","mark","delete"]

