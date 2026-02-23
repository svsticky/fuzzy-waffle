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
import qualified Data.ByteString.Char8 as B
import Network.HTTP.Client.TLS
import Network.HTTP.Client
import Network.URI as N
import OpenID.Connect.Client.Provider as O
import OpenID.Connect.Client.Flow.AuthorizationCode
import Data.String (IsString(fromString))
import Data.Maybe (fromJust)
import Web.Cookie

layout :: Markup -> Markup
layout body = html $ do
    H.head $ do
        title (text "My website!")
    H.body body -- add nav bar

page :: Markup
page = layout $ do
    h1 (text "Hello world!")
    form $ H.span $ toHtml ("foo" :: String)

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
    let port = 8080
    let settings = setPort port $ setHost host defaultSettings
    putStrLn $ "listening on " ++ show host ++ ":" ++ show port
    contents <- (decode <$> BSL.readFile "./data.json") :: IO (Maybe [Bet])
    -- print contents
    manager <- newTlsManager
    runSettings settings (app manager)

app :: Manager -> Application
app manager req res = do
    let headers = W.requestHeaders req
    print headers
    eitherDisc <- O.discovery (`httpLbs` manager) (N.URI "https:" (Just $ N.URIAuth "" "koala.dev.svsticky.nl" "") "" "" "")
    let ar = defaultAuthenticationRequest "openid member-read email profile" (Credentials "e1aacf592e92eedf71dea19f8c40b5ea5d51ebe3e30b29b843cfc05cb91aadd6" (AssignedSecretText "86b191438b892b9cac2e8d3e4db5f96a711628c49704366e78d3ebde98c28380b") (N.URI "" Nothing "login" "" "" `N.relativeTo` N.URI "http:" (Just $ N.URIAuth "" "0.0.0.0" ":8080") "" "" ""))
    print $ authRequestRedirectURI ar
    case eitherDisc of
        Left err -> error $ show err
        Right (discov,time') -> do
            case lookup "Set-Cookie" headers of
                Just c -> case rawPathInfo req of
                    "/" -> res $ responseLBS status200 [("Content-Type", "text/html")] (renderHtml page)
                    "/login" -> do
                        putStrLn "aaaaa"
                        let qry = W.queryString req
                        print qry
                        let code = case lookup "code" qry of
                                Just (Just s) -> s
                                _ -> error "no code found"
                        print code
                        let state = case lookup "state" qry of
                                Just (Just s) -> s
                                _ -> error "no state found"
                        print state
                        kys' <- keysFromDiscovery (`httpLbs` manager) discov
                        print kys'
                        let kys = case kys' of
                                Left err -> error $ show err
                                Right k -> fst k
                        print kys
                        let val = case lookup "oauth" $ parseCookies c of
                                Just v -> v
                                Nothing -> error "no cookie set"
                        print val
                        authResult <- authenticationSuccess (`httpLbs` manager) (fromJust time') (Provider discov kys) (Credentials "e1aacf592e92eedf71dea19f8c40b5ea5d51ebe3e30b29b843cfc05cb91aadd6" (AssignedSecretText "86b191438b892b9cac2e8d3e4db5f96a711628c49704366e78d3ebde98c28380b") (N.URI "http:" (Just $ N.URIAuth "" "0.0.0.0" ":8080") "" "" "")) (UserReturnFromRedirect val code state)
                        case authResult of
                            Left err -> error $ show err
                            Right claims -> error "works"
                    _ -> res notFoundRoute
                Nothing -> case rawPathInfo req of
                    "/" -> do
                        eitherRedirect <- authenticationRedirect discov ar
                        case eitherRedirect of
                            Left err -> error $ show err
                            Right (RedirectTo uri f) -> do
                                let cookie = f "oauth"
                                res $ mapResponseHeaders (("Set-Cookie", B.pack $ show cookie) :) $ responseLBS status302 [("Location",fromString $ show uri)] empty
                    _ -> res notFoundRoute


notFoundRoute :: W.Response
notFoundRoute =
    responseLBS
    status404
    [(hContentType, "application/json")]
    "404 - Not Found"