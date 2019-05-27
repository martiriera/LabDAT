
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
import           Control.Monad
import           Data.Time

{---------------------------------------------------------------------
                TODO
---------------------------------------------------------------------}

themeForm :: Maybe Theme -> AForm (HandlerFor Forum) Theme
themeForm maybeth =
    Theme <$> freq (checkM checkUserExists textField)
                   (withPlaceholder "Introduiu el nom de l'usuari responsable" "Nom del responsable")
                   (tLeader <$> maybeth)
          <*> pure ""
          <*> freq textField (withPlaceholder "Introduiu el títol del tema" "Titol del tema") (tTitle <$> maybeth)
          <*> freq textareaField (withPlaceholder "Introduiu la descripció del tema" "Descripció") (tDescription <$> maybeth)

questionForm :: ThemeId -> AForm (HandlerFor Forum) Question
questionForm tid =
    Question <$> pure tid
           <*> liftToAForm requireAuthId --converteix accio del handler a un AForm --requireAuthId retorna autenticador o aborta
           <*> liftToAForm (liftIO getCurrentTime)
           <*> freq textField (withPlaceholder "Introduïu el títol de la pregunta" "Assumpte") Nothing
           <*> freq textareaField (withPlaceholder "Introduïu la descripció de la pregunta" "Descripció") Nothing
             --freq consrueix form amb camp obligatori

answerForm :: QuestionId -> AForm (HandlerFor Forum) Answer
answerForm qid =
   Answer <$> pure qid
          <*> liftToAForm requireAuthId --converteix accio del handler a un AForm --requireAuthId retorna autenticador o aborta
          <*> liftToAForm (liftIO getCurrentTime)
          <*> freq textField (withPlaceholder "Introduïu la resposta" "Resposta") Nothing

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
    tformw <- generateAFormPost (themeForm Nothing)
    -- Return HTML content
    defaultLayout $ $(widgetTemplFile "src/forum/templates/home.html")

    -- isUpdate <- isJust <$> lookupPostParam "update"
    -- if isUpdate then do
    --
    -- else

postHomeR :: HandlerFor Forum Html
postHomeR = do
    user <- requireAuthId
    db <- getsSite forumDb
    (tformr, tformw) <- runAFormPost (themeForm Nothing)
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
    Just theme <- liftIO $ getTheme tid db
    questions <- liftIO $ getQuestionList tid db
    qformw <- generateAFormPost (questionForm tid)
    defaultLayout $(widgetTemplFile "src/forum/templates/currentTheme.html")


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

getThemeEditR :: ThemeId -> HandlerFor Forum Html
getThemeEditR tid = do
    -- Get model info
    db <- getsSite forumDb
    Just theme <- liftIO $ getTheme tid db
    user <- requireAuthId
    when (not (isLeader theme user)) (permissionDenied "L'usuari no és el moderador")
    tformw <- generateAFormPost (themeForm (Just theme))
    -- Return HTML content
    defaultLayout $ $(widgetTemplFile "src/forum/templates/updateTheme.html")

postThemeEditR :: ThemeId -> HandlerFor Forum Html
postThemeEditR tid = do
  user <- requireAuthId
  db <- getsSite forumDb
  Just theme <- liftIO $ getTheme tid db
  (tformr, tformw) <- runAFormPost (themeForm (Just theme))
  case tformr of
      FormSuccess edittheme -> do
          liftIO $ updateTheme tid edittheme db
          redirectRoute (ThemeEditR tid) []
      _ -> do
          let mbuser = Just user
          defaultLayout $(widgetTemplFile "src/forum/templates/updateTheme.html")


getQuestionR :: ThemeId -> QuestionId -> HandlerFor Forum Html
getQuestionR tid qid = do
      -- fail "A completar per l'estudiant"
      db <- getsSite forumDb
      mbuser <- maybeAuthId
      Just theme <- liftIO $ getTheme tid db
      Just question <- liftIO $ getQuestion qid db
      answers <- liftIO $ getAnswerList qid db
      aformw <- generateAFormPost (answerForm tid)
      defaultLayout $(widgetTemplFile "src/forum/templates/currentQuestion.html")

      --isJust :: Maybe a -> Bool
      --returns True if the argument is Just
      -- lookupPostParam :: Text -> m (Maybe Text)
      -- lookupPostParams :: m [Text]
      --
      -- isDelete <- isJust <$> lookupPostParam "delete"
      -- if isDelete then do
      --
      -- else

postQuestionR :: ThemeId -> QuestionId -> HandlerFor Forum Html
postQuestionR tid qid = do
  db <- getsSite forumDb
  user <- requireAuthId
  (aformr, aformw) <- runAFormPost (answerForm qid)
  Just theme <- liftIO $ getTheme tid db
  Just question <- liftIO $ getQuestion qid db
  answers <- liftIO $ getAnswerList qid db
  case aformr of
      FormSuccess newanswer -> do
          liftIO $ addAnswer newanswer db
          redirectRoute (QuestionR tid qid) []
      _ -> do
          questions <- liftIO $ getAnswerList qid db
          let mbuser = Just user
          defaultLayout $(widgetTemplFile "src/forum/templates/currentQuestion.html")

-- mapM :: Monad m => (a -> m b ) -> [a] -> m [b]
-- mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
-- for_ :: Monad m => [a] -> (a -> m b) -> m ()

-- for list $ \ cp -> do
--   case ... of
--       Nothing -> pure ()
--       Just aid -> liftIO $ deleteAnswer aid db
