{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types

main :: IO ()
main = do
    let settings = setPort 8080 $ setHost "192.168.2.31" defaultSettings 
    putStrLn "listening"
    runSettings settings app
    
app :: Application
app req res =
    res $ case rawPathInfo req of
        "/" -> helloRoute
        _ -> notFoundRoute
        
helloRoute :: Response
helloRoute = 
    responseLBS
    status200
    []
    "Hello, world!"
    
notFoundRoute :: Response
notFoundRoute =
    responseLBS
    status404
    [(hContentType, "application/json")]
    "404 - Not Found"