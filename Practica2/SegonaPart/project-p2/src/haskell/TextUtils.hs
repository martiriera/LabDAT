
{-# LANGUAGE OverloadedStrings #-}

module TextUtils
where
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding as T
import           Text.Read

------------------------------------------------------------------------
-- Show and Read conversions to/from Text

showtPrec :: Show a => Int -> a -> Text
showtPrec p x = T.pack $ showsPrec p x ""

showt :: Show a => a -> Text
showt = T.pack . show

readt :: Read a => Text -> Maybe a
readt = readMaybe . T.unpack

readtEither :: Read a => Text -> Either Text a
readtEither = either (Left . T.pack) Right . readEither . T.unpack

