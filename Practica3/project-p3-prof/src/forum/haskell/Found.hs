
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Found
where
import Model

import Develop.DatFw
import Develop.DatFw.Handler
import Develop.DatFw.Widget
import Develop.DatFw.Template
import Develop.DatFw.Auth

import Network.Wai.Middleware.Approot(getApproot)

import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.ByteString.Builder
import Data.Int


data Forum = Forum { forumDb :: ForumDb
                   , forumUsers :: [(Text, Text)]
                   }


data instance Route Forum =
          HomeR | ThemeR ThemeId | QuestionR ThemeId QuestionId
        | AuthR (Route Auth)

instance RenderRoute Forum where
    renderRoute HomeR   = ([], [])
    renderRoute (ThemeR tid) = (["themes",toPathPiece tid], [])
    renderRoute (QuestionR tid qid) = (["themes",toPathPiece tid,"qs",toPathPiece qid], [])
    renderRoute (AuthR authr) = let (path,qs) = renderRoute authr in ("auth":path, qs)

-- Nota: Els tipus ThemeId i QuestionId són alias de Int64 (veieu el model)
instance PathPiece Int64 where
    toPathPiece = showToPathPiece
    fromPathPiece = readFromPathPiece


instance WebApp Forum where
    appRoot _ req = T.decodeUtf8 $ getApproot req
    defaultLayout wdgt = do
        page <- widgetToPageContent wdgt
        mbmsg <- getMessage
        mbuser <- maybeAuthId
        applyUrlRenderTo $(htmlTemplFile "src/forum/templates/default-layout.html")
{---------------------------------------------------------------------
                TODO
    isAuthorized route isWrite = ...
---------------------------------------------------------------------}
    authRoute _ = Just $ AuthR LoginR     -- get the login link

instance WebAuth Forum where
    loginDest = HomeR
    logoutDest = HomeR
    validatePassword name password = do
        users <- getsSite forumUsers
        case lookup name users of
            Nothing    -> pure False
            Just upass -> pure $ upass == password

isAdmin :: UserId -> Bool
isAdmin u = u == "admin"


