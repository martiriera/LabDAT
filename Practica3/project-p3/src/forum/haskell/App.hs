
{-# LANGUAGE OverloadedStrings #-}

module App
where
import Found
import Model
import Handler

import Develop.DatFw
import Develop.DatFw.Dispatch
import Develop.DatFw.Auth

import Network.Wai

-- ---------------------------------------------------------------
-- Application initialization

-- NOTA: Canvieu al vostre fitxer de la base de dades
forumDbName = "/home/pract/LabDAT/ldatusr14/Practica3/project-p3/src/forum/sqlite/forum"

makeApp :: IO Application
makeApp = do
    -- Open the database (the model state)
    db <- openExistingDb forumDbName
    toApp Forum{ forumDb = db
               , forumUsers = [("admin", "piñas"), ("user1", "piñas"), ("user2", "piñas")]
               }

-- ---------------------------------------------------------------
-- Main controller

instance Dispatch Forum where
    dispatch = routing
            $ route ( onStatic [] ) HomeR
                [ onMethod "GET" getHomeR
                , onMethod "POST" postHomeR
                ]
            <||> route ( onStatic ["themes"] <&&> onDynamic ) ThemeR
                [ onMethod1 "GET" getThemeR
                , onMethod1 "POST" postThemeR
                ]
            <||> route ( onStatic ["themes"] <&&> onDynamic <&&> onStatic ["update"]) ThemeEditR
                [ onMethod1 "GET" getThemeEditR
                , onMethod1 "POST" postThemeEditR
                ]
            <||> route ( onStatic ["themes"] <&&> onDynamic <&&> onStatic ["qs"] <&&> onDynamic ) QuestionR
                [ onMethod2 "GET" getQuestionR
                , onMethod2 "POST" postQuestionR
                ]
            <||> routeSub (onStatic ["auth"]) AuthR getAuth
