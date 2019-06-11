
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Handler
where
import Found
import Model
import Json

import Develop.DatFw
import Develop.DatFw.Handler
import Develop.DatFw.Widget
import Develop.DatFw.Auth2

import Network.Wai

import           Control.Monad            -- imports forM_, ...
import           Control.Monad.IO.Class   -- imports liftIO
import           Control.Monad.Reader
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Maybe               -- imports isJust, isNothing, catMaybes, ...
import           Data.Monoid
import           Data.Bool                -- imports bool
import           Text.Read (readMaybe)
import           Data.Time

-- ---------------------------------------------------------------
-- Conversion to JSON representation
-- ---------------------------------------------------------------

themeToJSON :: (ThemeId, Theme) -> HandlerFor Forum Value
themeToJSON (tid, Theme{..}) = do
    ur <- getUrlRenderNoParams
    pure $ object [("selfLink", toJSON $ ur $ ThemeR tid),
                   ("leader", toJSON tLeader), ("category", toJSON tCategory),
                   ("title", toJSON tTitle), ("description", toJSON tDescription)]

questionToJSON :: ThemeId -> (QuestionId, Question) -> HandlerFor Forum Value
questionToJSON tid (qid, Question{..}) = do
    ur <- getUrlRenderNoParams
    pure $ object [("selfLink", toJSON $ ur $ QuestionR tid qid),
                   ("themeLink", toJSON $ ur $ ThemeR tid),
                   ("user", toJSON qUser), ("posted", toJSON qPosted),
                   ("title", toJSON qTitle), ("text", toJSON qText)]


answerToJSON :: ThemeId -> QuestionId -> (AnswerId, Answer) -> HandlerFor Forum Value
answerToJSON tid qid (aid, Answer{..}) = do
    ur <- getUrlRenderNoParams
    pure $ object [("selfLink", toJSON $ ur $ AnswerR tid qid aid),
                   ("questionLink", toJSON $ ur $ QuestionR tid qid),
                   ("user", toJSON aUser), ("posted", toJSON aPosted), ("text", toJSON aText)]

-- ---------------------------------------------------------------
-- Parsing JSON request body
-- ---------------------------------------------------------------

getRequestJSON :: (Object -> Parser a) -> HandlerFor Forum a
getRequestJSON fromObject = do
    jval <- getRequestJSON'
    case parseEither (withObject "Request body as an object" fromObject) jval of
        Left err -> invalidArgs [T.pack err]
        Right v -> pure v

getRequestJSON' :: HandlerFor Forum Value
getRequestJSON' = do
    req <- getRequest
    cType <- maybe (invalidArgs ["Request body with application/json content expected"])
                   (pure . T.decodeUtf8)
                   (lookup "content-type" (requestHeaders req))
    let isJson = "application/json" == fst (T.break (';' ==) cType)
    when (not isJson) $
        invalidArgs ["Request body with application/json content expected"]
    lbytes <- liftIO $ strictRequestBody req
    case eitherDecode lbytes of
        Left err -> invalidArgs [T.pack err]
        Right v -> pure v


-- ---------------------------------------------------------------
-- Utility: Run an IO action with the database
-- ---------------------------------------------------------------

runDbAction :: (ForumDb -> IO a) -> HandlerFor Forum a
runDbAction f = do
    db <- getsSite forumDb
    liftIO $ f db


-- ---------------------------------------------------------------
-- Handlers
-- ---------------------------------------------------------------

-- ---------------------------------------------------------------
-- User

getUserR :: HandlerFor Forum Value
getUserR = do
    -- Get model info
    mbuser <- maybeAuthId
    case mbuser of
        Nothing    -> notFound
        Just uname -> pure $ object [("name", toJSON uname), ("isAdmin", toJSON $ isAdmin uname)]

-- ---------------------------------------------------------------
-- Themes list

getThemesR :: HandlerFor Forum Value
getThemesR = do
    -- Get model info
    themes <- runDbAction getThemeList
    jthemes <- mapM themeToJSON themes
    pure $ object [("items", toJSON jthemes)]

postThemesR :: HandlerFor Forum Value
postThemesR = do
    user <- requireAuthId
    (pLeader, pTitle, pDescription) <- getRequestJSON $ \ obj -> do
        leader <- obj .: "leader"
        title <- obj .: "title"
        description <- obj .: "description"
        pure (leader, title, description)
    when (not $ isAdmin user) (permissionDenied "User is not an admin")
    let newtheme = Theme pLeader "" pTitle pDescription
    tid <- runDbAction $ addTheme newtheme
    getThemeR tid

-- ---------------------------------------------------------------
-- Theme

getThemeR :: ThemeId -> HandlerFor Forum Value
getThemeR tid = do
    theme <- runDbAction (getTheme tid) >>= maybe notFound pure
    themeToJSON (tid, theme)

deleteThemeR :: ThemeId -> HandlerFor Forum Value
deleteThemeR tid = do
    user <- requireAuthId
    theme <- runDbAction (getTheme tid) >>= maybe notFound pure
    when (not $ isAdmin user) (permissionDenied "User is not an admin")
    runDbAction $ deleteFullTheme tid
    pure $ object []

deleteFullTheme :: ThemeId -> ForumDb -> IO ()
deleteFullTheme tid db = do
    ... TODO ...

-- ---------------------------------------------------------------
-- Questions list

getQuestionsR :: ThemeId -> HandlerFor Forum Value
getQuestionsR tid = do
    ... TODO ...

postQuestionsR :: ThemeId -> HandlerFor Forum Value
postQuestionsR tid = do
    ... TODO ...

-- ---------------------------------------------------------------
-- Question

getQuestionR :: ThemeId -> QuestionId -> HandlerFor Forum Value
getQuestionR tid qid = do
    ... TODO ...

deleteQuestionR :: ThemeId -> QuestionId -> HandlerFor Forum Value
deleteQuestionR tid qid = do
    ... TODO ...

-- ---------------------------------------------------------------
-- Answers list

getAnswersR :: ThemeId -> QuestionId -> HandlerFor Forum Value
getAnswersR tid qid = do
    ... TODO ...

postAnswersR :: ThemeId -> QuestionId -> HandlerFor Forum Value
postAnswersR tid qid = do
    ... TODO ...

-- ---------------------------------------------------------------
-- Answer

getAnswerR :: ThemeId -> QuestionId -> AnswerId -> HandlerFor Forum Value
getAnswerR tid qid aid = do
    ... TODO ...

deleteAnswerR :: ThemeId -> QuestionId -> AnswerId -> HandlerFor Forum Value
deleteAnswerR tid qid aid = do
    ... TODO ...


