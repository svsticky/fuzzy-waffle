{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module KBG.Config (EnvCredentials(..), loadEnvCredentials) where

import Network.Wai
import Network.HTTP.Client (responseBody, Manager, httpLbs, parseRequest, applyBasicAuth, urlEncodedBody)
import Network.HTTP.Types
import Data.Maybe (fromJust, isNothing, isJust, maybeToList)

import Data.ByteString (StrictByteString, breakSubstring)
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Char8 as C8

import Data.Aeson.Types (typeMismatch)
import Data.Aeson
import Control.Monad
import System.Random
import System.Environment (lookupEnv)
import Configuration.Dotenv (loadFile, defaultConfig, configPath)


data EnvCredentials = EnvCredentials
    { clientId     :: StrictByteString
    , clientSecret :: StrictByteString
    , host         :: String
    , port         :: Int
    , baseURL      :: StrictByteString
    , databaseURL  :: String
    , oAuthReq     :: String
    }

loadEnvCredentials :: IO EnvCredentials
loadEnvCredentials = do
    loadFile defaultConfig { configPath = [".env"] }
    clientId_     <- lookupEnv "OAUTH_CLIENT_ID"
    clientSecret_ <- lookupEnv "OAUTH_CLIENT_SECRET"
    host_         <- lookupEnv "HOST"
    port_         <- lookupEnv "PORT"
    databaseURL_  <- lookupEnv "DATABASE_URL"
    oAuthReq_     <- lookupEnv "OAUTH_REQ_URL"
    case (clientId_, clientSecret_, host_, port_, databaseURL_, oAuthReq_) of
        (Just cid, Just sec, Just hst, Just prt, Just dbURL, Just oAuthReqURL) -> return $ 
            EnvCredentials (C8.pack cid) (C8.pack sec) hst (read prt) (C8.pack hst <> ":" <> C8.pack prt) dbURL oAuthReqURL
        _                                         -> fail "Incomplete .env file"
