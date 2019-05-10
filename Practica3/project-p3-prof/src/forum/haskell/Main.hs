
{-# LANGUAGE OverloadedStrings #-}

module Main
where
import App

import Network.Wai
import Network.Wai.Handler.CGI(run)
import Network.Wai.Middleware.Approot(envFallbackNamed)

import Control.Exception

-- ****************************************************************

main :: IO ()
main = do
    -- La funcio 'makeApp' (definida en el modul App) construeix una aplicacio WAI
    -- a partir d'una aplicacio de tipus Forum (instancia de WebApp de DatFw)
    r <- try makeApp
    case r of
        Right app -> do
            -- CGI adapter
            appRootMWare <- envFallbackNamed "SCRIPT_NAME"
            run $ appRootMWare app
        Left exc -> do
            -- Exception on initialization
            putStrLn "Status: 500 Internal Server Error"
            putStrLn "Content-Type: text/plain"
            putStrLn ""
            putStrLn "Exception on initialization (while excution of 'makeApp'): "
            putStrLn $ "    " ++ show (exc :: SomeException)

