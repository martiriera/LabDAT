
{-# LANGUAGE OverloadedStrings #-}

module WaiUtils
where
import           Network.Wai
import           Network.HTTP.Types
import           Web.Cookie

import           Data.Monoid
import           Data.Maybe
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding as T
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy as BL
import           Text.Read
import           Control.Monad
import           Control.Applicative

-- ****************************************************************
-- Utilities

requestGetPostQuery :: Request -> IO Query
requestGetPostQuery req =
    parseQuery <$> getBody req
    where
        getBody req = do
            b <- requestBody req
            if B.null b then pure B.empty
            else do
                bs <- getBody req
                pure $ b <> bs

lookupParam :: Text -> Query -> Maybe Text
lookupParam name query =
    case lookupParams name query of
        []    -> Nothing
        (x:_) -> Just x

lookupParams :: Text -> Query -> [Text]
lookupParams name query =
    let nameBS = T.encodeUtf8 name
    in T.decodeUtf8 <$> catMaybes (snd <$> filter ((==) nameBS . fst) query)

mimeHtml :: B.ByteString
mimeHtml = "text/html;charset=UTF-8"

htmlResponse :: Text -> [(Text, Text)] -> Response
htmlResponse html session =
    let headers = [ ("Content-Type", mimeHtml)
                  , ("Set-Cookie", mkSetCookieValue session) ]
    in responseBuilder ok200 headers (T.encodeUtf8Builder html)

redirectResponse :: Text -> [(Text, Text)] -> Response
redirectResponse url session =
    let headers = [ ("Location", T.encodeUtf8 url)
                  , ("Content-Type", "text/plain;charset=UTF-8")
                  , ("Set-Cookie", mkSetCookieValue session) ]
    in responseBuilder seeOther303 headers (T.encodeUtf8Builder "Redirect")

requestSession :: Request -> [(Text, Text)]
requestSession req =
    let mbvalue = do
            cookieHeader <- lookup "Cookie" (requestHeaders req)
            session <- lookup (T.encodeUtf8 "session") (parseCookies cookieHeader)
            readMaybe $ T.unpack $ T.decodeUtf8 session
    in maybe [] id mbvalue

mkSetCookieValue :: [(Text, Text)] -> B.ByteString
mkSetCookieValue session =
    let setCookie = defaultSetCookie { setCookieName = T.encodeUtf8 "session"
                                     , setCookieValue = T.encodeUtf8 $ T.pack $ show session
                                     }
    in BL.toStrict $ toLazyByteString $ renderSetCookie setCookie

errorResponse :: Status -> Text -> Response
errorResponse status msg =
    let headers = [ ("Content-Type", mimeHtml) ]
        html = "<!DOCTYPE html><html><head><title>Exemple CGI Lib: Tasques</title></head><body>\n"
                <> "<center><h2>ERROR</h2><h3><font color=\"red\">" <> msg <> "</font></h3></center>\n"
                <> "</body></html>\n"
    in responseBuilder status headers (T.encodeUtf8Builder html)
