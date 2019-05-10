
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Found
where
import Model

import Develop.DatFw
import Develop.DatFw.Template
import Develop.DatFw.Auth

import Network.Wai.Middleware.Approot(getApproot)

import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.ByteString.Builder
import Data.Int


-- ****************************************************************
-- Definicions basiques de l'aplicacio o lloc Tasks:
--      Tipus Tasks (informacio global del lloc): conte la connexio amb la base de dades
--      Tipus de les rutes de Tasks
--      Instancia de WebApp (configuracio del lloc) per a Tasks
--      Instancia de WebAuth (configuracio del subsistema d'autenticacio Auth) per a Tasks

-- NOTA: Veieu en <https://en.wikibooks.org/wiki/Haskell/More_on_datatypes#Named_Fields_(Record_Syntax)>
-- una breu introducció a la sintaxis dels registres Haskell

data Tasks = Tasks { tasksDb :: TaskDb }

data instance Route Tasks =
          HomeR
        | AuthR (Route Auth)

type TasksRoute = Route Tasks

instance RenderRoute Tasks where
    renderRoute HomeR   = ([], [])
    renderRoute (AuthR authr) = let (path, qs) = renderRoute authr in ("auth" : path, qs)


instance WebApp Tasks where
    appRoot _ req = T.decodeUtf8 $ getApproot req
    defaultLayout wdgt = do
        page <- widgetToPageContent wdgt
        mbuser <- maybeAuthId
        mbmsg <- getMessage
        applyUrlRenderTo $(htmlTemplFile "src/tasks/templates/default-layout.html")

instance WebAuth Tasks where
    loginDest = HomeR
    logoutDest = HomeR
    validatePassword name password =
        pure $ name /= "" && name == password

