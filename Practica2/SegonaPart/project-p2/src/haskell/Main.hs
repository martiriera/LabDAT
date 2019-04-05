
{-# LANGUAGE OverloadedStrings #-}

module Main
where
import App
{-
-- Si voleu provar la versio previa de l'aplicacio,
-- importeu el modul 'App_prev' en lloc del modul 'App'.
import App_prev
-}

import Network.Wai
import Network.Wai.Handler.CGI(run)

import Control.Exception

-- ****************************************************************

main :: IO ()
main = do
    r <- try $
        -- CGI adapter
        run calcApp
    case r of
        Right _ -> pure ()
        Left exc -> do
            -- Exception on initialization
            putStrLn "Status: 500 Internal Server Error"
            putStrLn "Content-Type: text/plain"
            putStrLn ""
            putStrLn "Exception on initialization (while excution of 'calcApp'): "
            putStrLn $ "    " ++ show (exc :: SomeException)

