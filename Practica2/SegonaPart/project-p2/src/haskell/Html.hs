
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Html
where
import           Data.Monoid
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy as BL
import           Text.Read
import           Control.Monad
import           Control.Applicative


newtype HtmlM a = HtmlM { runHtmlM :: (a, [Text]) }

type Html = HtmlM ()

instance Functor HtmlM where
    fmap f (HtmlM (x, out)) = HtmlM ( f x, out )

instance Applicative HtmlM where
    pure x = HtmlM (x, [])
    HtmlM (f, outf) <*> HtmlM (x, outx) = HtmlM ( f x, outf <> outx )

instance Monad HtmlM where
    return = pure
    HtmlM (x, out1) >>= f = HtmlM $
        let HtmlM (y, out2) = f x
        in (y, out1 <> out2)

instance Monoid (HtmlM ()) where
    mempty = pure ()
    mappend (HtmlM (_, out1)) (HtmlM (_, out2)) =
        HtmlM ((), out1 <> out2)

runHtml :: Html -> [Text]
runHtml = snd . runHtmlM

runHtmlToText :: Html -> Text
runHtmlToText = T.concat . concatMap addLF . snd . runHtmlM
    where
        addLF t = [t, "\n"]

_hTell :: [Text] -> Html
_hTell lines = HtmlM ((), lines)

hDOCTYPE :: Html
hDOCTYPE = _hTell ["<!DOCTYPE html>"]

hText :: Text -> Html
hText t = _hTell [_escapeHtml t]

hElem :: Text -> [(Text, Text)] -> HtmlM a -> HtmlM a
hElem name attrs content = HtmlM $
        let (x, lines) = runHtmlM content
        in ( x, ("<" <> name <> foldMap _outAttr attrs <> ">")
                : fmap ("  " <>) lines
                <> [ "</" <> name <> ">" ]
           )

hElemEmpty :: Text -> [(Text, Text)] -> Html
hElemEmpty name attrs = HtmlM $
        ( (), [ "<" <> name <> foldMap _outAttr attrs <> ">" ] )

_outAttr :: (Text, Text) -> Text
_outAttr (name,value) = " " <> name <> "='" <> _escapeHtml value <> "'"

_escapeHtml :: Text -> Text
_escapeHtml =
    let convert '<'  = "&lt;"
        convert '&'  = "&amp;"
        convert '>'  = "&gt;"
        convert '\"' = "&quot;"
        convert '\'' = "&apos;"
        convert c    = T.singleton c
    in T.concatMap convert
