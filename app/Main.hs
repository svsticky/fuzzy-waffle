{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Network.Wai as W
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Client.TLS
import Data.Aeson

import KBG.Auth

layout :: Markup -> Markup
layout body = H.html $ do
    H.head $ do
        H.title (text "My website!")
    H.body body -- add nav bar

page :: Markup
page = layout $ do
    H.h1 (text "Hello world!")
    H.form $ H.span $ toHtml ("foo" :: String)

data Bettee = Bettee
    { name :: String
    , function :: String
    } deriving (Show, Generic, FromJSON)

data Bet = Bet
    { better :: String
    , bettees :: [Bettee]
    } deriving (Show, Generic, FromJSON)

main :: IO ()
main = do
    let host = "0.0.0.0"
    let port = 3000
    let settings = setPort port $ setHost host defaultSettings
    putStrLn $ "listening on " ++ show host ++ ":" ++ show port
    contents <- (decode <$> BSL.readFile "./data.json") :: IO (Maybe [Bet])
    -- print contents
    manager <- newTlsManager
    disc <- discovery manager
    runSettings settings (requireSession manager disc app)

app :: Application
app req res = res $ responseLBS status200 [] "hello"

notFoundRoute :: W.Response
notFoundRoute =
    responseLBS
    status404
    [(hContentType, "application/json")]
    "404 - Not Found"