{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use catMaybes" #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# HLINT ignore "Use bimap" #-}
module Main where

import Network.Wai as W
import Network.Wai.Handler.Warp
import Network.Wai.Parse
import Network.HTTP.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Network.HTTP.Client.TLS
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.ByteString.Char8 as C8
import Text.Read (readMaybe)
import Data.Time (getCurrentTime)
import Data.IORef
import qualified Data.Map.Strict as Map

import KBG.Config (loadEnvCredentials, EnvCredentials(..))
import KBG.Auth (discovery, requireSession, parseCookies, SessionStore)
import KBG.Layout (renderPage)
import KBG.Pages.NotFound
import KBG.Pages.Admin
import KBG.Pages.Index
import KBG.DB (withDb, initDb, saveSubmission, getAllSubmissions, getSubmissionByUserId)

main :: IO ()
main = do
    putStrLn "[startup] Loading environment..."
    envCreds <- loadEnvCredentials
    putStrLn "[startup] Connecting to database..."
    withDb envCreds.databaseURL initDb
    putStrLn "[startup] Database ready"
    subs <- withDb envCreds.databaseURL getAllSubmissions
    putStrLn $ "[startup] Existing submissions: " ++ show (length subs)
    putStrLn "[startup] Fetching OIDC discovery document..."
    manager <- newTlsManager
    disc <- discovery manager envCreds
    putStrLn "[startup] OIDC discovery done"
    putStrLn $ "[startup] Server started at 0.0.0.0:" ++ show envCreds.port
    let settings = setPort envCreds.port
                 $ setHost "0.0.0.0"
                 $ setLogger logRequest
                 $ defaultSettings
    runSettings settings (requireSession manager disc envCreds (app envCreds))

logRequest :: Request -> Status -> Maybe Integer -> IO ()
logRequest req status _ = do
    now <- getCurrentTime
    putStrLn $ "[" ++ show now ++ "] "
        ++ C8.unpack (requestMethod req)
        ++ " "
        ++ C8.unpack (rawPathInfo req)
        ++ " -> "
        ++ show (statusCode status)

app :: EnvCredentials -> Application
app envCreds req res = do
    let session = getSession req
        userId = fst <$> session
        admin = maybe False snd session
    case (requestMethod req, pathInfo req) of
        ("GET", path) | path `elem` [[], ["callback"]] -> do
            existing <- case userId of
                Nothing  -> return []
                Just uid -> withDb envCreds.databaseURL (getSubmissionByUserId uid)
            res $ htmlResponse status200 (renderPage (mainPage existing Nothing))
        ("POST", []) -> do
            (params, _) <- parseRequestBody lbsBackEnd req
            let fields = map (\(k,v) -> (C8.unpack k, C8.unpack v)) params
            case userId of
                Nothing  ->
                    putStrLn "[db] Could not determine user_id from session cookie"
                Just uid -> do
                    putStrLn $ "[db] Saving submission for user_id " ++ show uid
                    withDb envCreds.databaseURL $ \conn -> saveSubmission conn uid fields
                    putStrLn $ "[db] Saved submission for user_id " ++ show uid ++ ": " ++ show fields
            let submitted = map (\(f,_,_) -> (T.unpack f, maybe "" T.unpack (lookupParam f params))) formFields
            res $ htmlResponse status200 (renderPage (mainPage submitted (Just "Je inzending is opgeslagen!")))
        ("GET", ["admin"]) ->
            if admin
                then res $ htmlResponse status200 (renderPage adminPage)
                else res $ htmlResponse status403 (renderPage notFoundPage)
        _ -> res $ htmlResponse status404 (renderPage notFoundPage)

htmlResponse :: Status -> BSL.ByteString -> W.Response
htmlResponse status = responseLBS status [("Content-Type", "text/html; charset=utf-8")]

lookupParam :: Text -> [(BS.ByteString, BS.ByteString)] -> Maybe Text
lookupParam key params = decodeUtf8 <$> lookup (encodeUtf8_ key) params
    where encodeUtf8_ = Data.Text.Encoding.encodeUtf8

getSession :: Request -> Maybe (Int, Bool)
getSession req = do
    cookie <- lookup "Cookie" (requestHeaders req)
    val    <- lookup "session" (parseCookies cookie)
    case C8.split ':' val of
        [uid, adm] -> (,) <$> readMaybe (C8.unpack uid) <*> pure (adm == "1")
        _          -> Nothing