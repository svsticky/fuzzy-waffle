{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Network.Wai as W
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Data.Aeson (FromJSON, decode)
import GHC.Generics (Generic)
import Data.ByteString.Lazy as BSL
import Network.HTTP.Client.TLS
import Network.HTTP.Client
import Network.URI as N
import OpenID.Connect.Client.Provider as O
import OpenID.Connect.Client.Flow.AuthorizationCode
import Data.String (IsString(fromString))

layout :: Markup -> Markup
layout body = html $ do
    H.head $ do
        title (text "My website!")
    H.body body -- add nav bar

page :: Markup
page = layout $ do
    h1 (text "Hello world!")

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
    let settings = setPort 8080 $ setHost "0.0.0.0" defaultSettings 
    putStrLn "listening"
    contents <- (decode <$> BSL.readFile "./data.json") :: IO (Maybe [Bet])
    -- print contents
    manager <- newTlsManager
    runSettings settings (app manager)
    
app :: Manager -> Application
app manager req res = do
    let headers = W.requestHeaders req
    case lookup "Set-Cookie" headers of
        Just _ -> res $ case rawPathInfo req of
            "/" -> responseLBS status200 [("Content-Type", "text/html")] (renderHtml page)
            _ -> notFoundRoute
        Nothing -> do
            eitherDisc <- O.discovery (`httpLbs` manager) (N.URI "https:" (Just $ N.URIAuth "" "koala.dev.svsticky.nl" "") "" "" "")
            let ar = defaultAuthenticationRequest "openid member-read email profile" (Credentials "e1aacf592e92eedf71dea19f8c40b5ea5d51ebe3e30b29b843cfc05cb91aadd6" (AssignedSecretText "86b191438b892b9cac2e8d3e4db5f96a711628c49704366e78d3ebde98c28380b") (N.URI "http:" (Just $ N.URIAuth "" "0.0.0.0" ":8080") "" "" ""))
            case eitherDisc of
                Left err -> error $ show err
                Right (discovery,time') -> do 
                    eitherRedirect <- authenticationRedirect discovery ar
                    case eitherRedirect of
                        Left err -> error $ show err
                        Right (RedirectTo uri f) -> res $ responseLBS status307 [("Location",fromString $ show uri)] ""

notFoundRoute :: W.Response
notFoundRoute =
    responseLBS
    status404
    [(hContentType, "application/json")]
    "404 - Not Found"