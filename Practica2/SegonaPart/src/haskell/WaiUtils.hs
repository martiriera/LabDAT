
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

-- Obte els parametres de la peticio a partir del contingut de la peticio.
-- Aquesta accio sols es pot executar una sola vegada sobre la peticio,
-- ja que llegeix i consumeix el seu contingut.
-- El resultat es una representacio del corresponent conjunt de parelles (nom, valors);
-- per obtenir el(s) valor(s) corresponent(s) a un determinat nom caldra usar la
-- funcio 'lookupParam(s)'.
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

-- Obte l'estat de sessio a partir de la corresponent 'cookie' de la peticio.
requestSession :: Request -> [(Text, Text)]
requestSession req =
    let mbvalue = do
            cookieHeader <- lookup "Cookie" (requestHeaders req)
            session <- lookup (T.encodeUtf8 "session") (parseCookies cookieHeader)
            readMaybe $ T.unpack $ T.decodeUtf8 session
    in maybe [] id mbvalue


mimeHtml :: B.ByteString
mimeHtml = "text/html;charset=UTF-8"

-- Obte una resposta normal a partir del contingut (que es suposa text HTML) i de l'estat de sessio.
htmlResponse :: Text -> [(Text, Text)] -> Response
htmlResponse html session =
    let headers = [ ("Content-Type", mimeHtml)
                  , ("Set-Cookie", mkSetCookieValue session) ]
    in responseBuilder ok200 headers (T.encodeUtf8Builder html)

-- Obte una resposta de redireccio a partir de la URL de desti i de l'estat de sessio.
redirectResponse :: Text -> [(Text, Text)] -> Response
redirectResponse url session =
    let headers = [ ("Location", T.encodeUtf8 url)
                  , ("Content-Type", "text/plain;charset=UTF-8")
                  , ("Set-Cookie", mkSetCookieValue session) ]
    in responseBuilder seeOther303 headers (T.encodeUtf8Builder "Redirect")

-- Funcio auxiliar que obte el valor de la 'cookie' resultant a partir de l'estat de sessio.
mkSetCookieValue :: [(Text, Text)] -> B.ByteString
mkSetCookieValue session =
    let setCookie = defaultSetCookie { setCookieName = T.encodeUtf8 "session"
                                     , setCookieValue = T.encodeUtf8 $ T.pack $ show session
                                     }
    in BL.toStrict $ toLazyByteString $ renderSetCookie setCookie

-- Obte una resposta a partir del codi d'estat i un missatge d'error.
errorResponse :: Status -> Text -> Response
errorResponse status msg =
    let headers = [ ("Content-Type", "text/plain;charset=UTF-8") ]
    in responseBuilder status headers (T.encodeUtf8Builder msg)

