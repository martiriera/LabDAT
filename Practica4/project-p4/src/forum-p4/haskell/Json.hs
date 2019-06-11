
{-# LANGUAGE OverloadedStrings #-}

module Json
    ( module Json
    , module Data.Aeson
    , module Data.Aeson.Types
    , module Develop.DatFw.Content
    )
where

import Develop.DatFw.Content

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy as B
import Data.ByteString.Builder as B


instance ToContent Value where
    toContent json =
        let bs = encode json
        in ContentBuilder (B.lazyByteString bs) (Just $ fromIntegral $ B.length bs)

instance ToTypedContent Value where
    toTypedContent json = TypedContent typeJson (toContent json)

