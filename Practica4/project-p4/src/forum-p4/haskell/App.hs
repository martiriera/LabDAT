
{-# LANGUAGE OverloadedStrings #-}

module App
where
import Found
import Model
import Handler

import Develop.DatFw
import Develop.DatFw.Dispatch
import Develop.DatFw.Auth2

import Network.Wai

-- ---------------------------------------------------------------
-- Application initialization

-- NOTA: Canvieu al vostre fitxer de la base de dades
forumDbName :: Text
forumDbName = "/home/pract/LabWEB/WEBprofe/private/sqlite3-dbs/forum.db"

makeApp :: IO Application
makeApp = do
    -- Open the database (the model state)
    db <- openExistingDb forumDbName
    toApp Forum{ forumDb = db
               , forumUsers = [("admin", "201811"), ("user1", "1234"), ("user2", "1234")]
               }


-- ---------------------------------------------------------------
-- Main controller

instance Dispatch Forum where
    dispatch = routing $
            -- RESTful API:
            --  URI: /themes
            route ( onStatic ["themes"] ) ThemesR
                [ onMethod "GET" getThemesR             -- get the theme list
                , onMethod "POST" postThemesR           -- create a new theme
                ] <||>
            --  URI: /themes/TID
            route ( onStatic ["themes"] <&&> onDynamic ) ThemeR
                [ onMethod1 "GET" getThemeR             -- get a theme
                , onMethod1 "DELETE" deleteThemeR       -- delete a theme
                ] <||>
            --  URI: /themes/TID/questions
            route ( onStatic ["themes"] <&&> onDynamic <&&> onStatic ["questions"] ) QuestionsR
                [ onMethod1 "GET" getQuestionsR          -- get a theme's question list
                , onMethod1 "POST" postQuestionsR       -- create a new question
                ] <||>
            --  URI: /themes/TID/questions/QID
            route ( onStatic ["themes"] <&&> onDynamic <&&> onStatic ["questions"] <&&> onDynamic ) QuestionR
                [ onMethod2 "GET" getQuestionR          -- get a question
                , onMethod2 "DELETE" deleteQuestionR    -- delete a question
                ] <||>
            --  URI: /themes/TID/questions/QID/answers
            route ( onStatic ["themes"] <&&> onDynamic <&&> onStatic ["questions"] <&&> onDynamic <&&> onStatic ["answers"] ) AnswersR
                [ onMethod2 "GET" getAnswersR             -- get a question's answer list
                , onMethod2 "POST" postAnswersR         -- create a new answer
                ] <||>
            --  URI: /themes/TID/questions/QID/answers/AID
            route ( onStatic ["themes"] <&&> onDynamic <&&> onStatic ["questions"] <&&> onDynamic <&&> onStatic ["answers"] <&&> onDynamic ) AnswerR
                [ onMethod3 "GET" getAnswerR             -- get an answer
                , onMethod3 "DELETE" deleteAnswerR      -- delete an answer
                ] <||>
            --  URI: /user
            route ( onStatic ["user"] ) UserR
                [ onMethod "GET" getUserR               -- get the current user
                ] <||>
            -- Authentication interface:
            routeSub (onStatic ["auth"]) AuthR getAuth

