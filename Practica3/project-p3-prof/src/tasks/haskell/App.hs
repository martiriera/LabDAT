
{-# LANGUAGE OverloadedStrings #-}

module App
where
import Found
import Model
import Handler

import Develop.DatFw
import Develop.DatFw.Auth

import Network.Wai

-- ****************************************************************
-- Application initialization:
--      Construccio de l'estat de l'aplicacio Tasks i
--      corresponent instancia de Dispatch a partir de la definicio de rutes.
--      L'autenticacio d'usuaris es realitza en un subsistema separat Auth.

tasksDbName = "/home/pract/LabWEB/WEBprofe/private/sqlite3-dbs/tasks.db"

makeApp :: IO Application
makeApp = do
    -- Open the database (the model state)
    db <- openExistingDb tasksDbName
    toApp Tasks{ tasksDb = db }

-- ****************************************************************
-- Controller's main entry: route dispatching

instance Dispatch Tasks where
    dispatch = routing
            $ route ( onStatic [] ) HomeR
                [ onMethod "GET" getHomeR
                , onMethod "POST" postHomeR
                ]
            <||> routeSub (onStatic ["auth"]) AuthR getAuth

