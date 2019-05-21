
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler
where
import Found
import Model

import Develop.DatFw
import Develop.DatFw.Handler
import Develop.DatFw.Template
import Develop.DatFw.Auth
import Develop.DatFw.Form
import Develop.DatFw.Form.Fields


import           Data.Text (Text)
import           Control.Monad.IO.Class   -- imports liftIO
import           Data.Time

{---------------------------------------------------------------------
                TODO
---------------------------------------------------------------------}

themeForm :: AForm (HandlerFor Forum) Theme
themeForm =
    Theme <$> freq (checkM checkUserExists textField)
                   (withPlaceholder "Introduiu el nom de l'usuari responsable" "Nom del responsable")
                   Nothing
          <*> pure ""
          <*> freq textField (withPlaceholder "Introduiu el títol del tema" "Titol") Nothing
          <*> freq textareaField (withPlaceholder "Introduiu la descripció del tema" "Descripció") Nothing

questionForm :: ThemeId -> AForm (HandlerFor Forum) Question
questionForm tid =
    Question <$> pure tid
           <*> liftToAForm requireAuthId --converteix accio del handler a un AForm --requireAuthId retorna autenticador o aborta
           <*> liftToAForm (liftIO getCurrentTime)
           <*> freq textField (withPlaceholder "Introduïu el títol de la pregunta" "Títol") Nothing
           <*> freq textareaField (withPlaceholder "Introduïu la descripció de la pregunta" "Descripció") Nothing
             --freq consrueix form amb camp obligatori

answerForm :: QuestionId -> AForm (HandlerFor Forum) Answer
answerForm qid =
   Answer <$> pure qid
          <*> liftToAForm requireAuthId --converteix accio del handler a un AForm --requireAuthId retorna autenticador o aborta
          <*> liftToAForm (liftIO getCurrentTime)
          <*> freq textField (withPlaceholder "Introduïu el la resposta" "Resposta") Nothing

checkUserExists :: Text -> HandlerFor Forum (Either Text Text)
checkUserExists uname = do
    users <- getsSite forumUsers
    case lookup uname users of
        Nothing -> pure $ Left "L'usuari no existeix"
        Just _  -> pure $ Right uname

getHomeR :: HandlerFor Forum Html
getHomeR = do
    -- Get model info
    db <- getsSite forumDb
    themes <- liftIO $ getThemeList db
    mbuser <- maybeAuthId
    tformw <- generateAFormPost themeForm
    -- Return HTML content
    defaultLayout $ $(widgetTemplFile "src/forum/templates/home.html")

postHomeR :: HandlerFor Forum Html
postHomeR = do
    user <- requireAuthId
    db <- getsSite forumDb
    (tformr, tformw) <- runAFormPost themeForm
    case tformr of
      -- http://soft0.upc.edu/dat/datfw/haddock/datfw/Develop-DatFw-Form.html#v:runAFormPost
      -- S'ha de comprovar si el Form és success, missing o failure
        FormSuccess newtheme -> do
            liftIO $ addTheme newtheme db
            redirectRoute HomeR []
        _ -> do
            themes <- liftIO $ getThemeList db
            let mbuser = Just user
            defaultLayout $(widgetTemplFile "src/forum/templates/home.html")


getThemeR :: ThemeId -> HandlerFor Forum Html
getThemeR tid = do
    -- fail "A completar per l'estudiant"
    db <- getsSite forumDb
    mbuser <- maybeAuthId 
    -- let mbuser = Just user
    -- mbuser <- requireAuthId
    findtheme <- liftIO $ getTheme tid db
    questions <- liftIO $ getQuestionList tid db
    qformw <- generateAFormPost (questionForm tid)
    case findtheme of
      Nothing -> defaultLayout $(widgetTemplFile "src/forum/templates/themeNotFound.html")
      Just theme -> defaultLayout $(widgetTemplFile "src/forum/templates/currentTheme.html")


postThemeR :: ThemeId -> HandlerFor Forum Html
postThemeR tid = do
  user <- requireAuthId
  db <- getsSite forumDb
  (qformr, qformw) <- runAFormPost (questionForm tid)
  Just theme <- liftIO $ getTheme tid db
  case qformr of
      FormSuccess newquestion -> do
          liftIO $ addQuestion newquestion db
          redirectRoute (ThemeR tid) []
      _ -> do
          questions <- liftIO $ getQuestionList tid db
          let mbuser = Just user
          defaultLayout $(widgetTemplFile "src/forum/templates/currentTheme.html")

getQuestionR :: ThemeId -> QuestionId -> HandlerFor Forum Html
getQuestionR tid qid = do
      -- fail "A completar per l'estudiant"
      db <- getsSite forumDb
      Just theme <- liftIO $ getTheme tid db
      findquestion <- liftIO $ getQuestion qid db
      answers <- liftIO $ getAnswerList qid db
      aformw <- generateAFormPost (answerForm tid)
      case findquestion of
        Nothing -> defaultLayout $(widgetTemplFile "src/forum/templates/questionNotFound.html")
        Just question -> defaultLayout $(widgetTemplFile "src/forum/templates/currentQuestion.html")


postQuestionR :: ThemeId -> QuestionId -> HandlerFor Forum Html
postQuestionR tid qid = do
  db <- getsSite forumDb
  user <- requireAuthId
  (aformr, aformw) <- runAFormPost (answerForm qid)
  Just theme <- liftIO $ getTheme tid db
  Just question <- liftIO $ getQuestion qid db
  case aformr of
      FormSuccess newanswer -> do
          liftIO $ addAnswer newanswer db
          redirectRoute (QuestionR tid qid) []
      _ -> do
          questions <- liftIO $ getAnswerList qid db
          let mbuser = Just user
          defaultLayout $(widgetTemplFile "src/forum/templates/currentQuestion.html")
