{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module KBG.Auth (requireSession, discovery, Discovery, EnvCredentials(..), loadEnvCredentials, parseCookies) where

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

import KBG.Config (EnvCredentials(..), loadEnvCredentials)

data Discovery = Discovery
    { authorization_endpoint :: StrictByteString
    , token_endpoint :: StrictByteString
    }

instance FromJSON Discovery where
    parseJSON (Object o) = (\a b -> Discovery (C8.pack a) (C8.pack b))
        <$> o .: "authorization_endpoint"
        <*> o .: "token_endpoint"
    parseJSON invalid = typeMismatch "Object" invalid

discovery :: Manager -> EnvCredentials -> IO Discovery
discovery man envCreds = do
    req <- parseRequest envCreds.oAuthReq
    fromJust . decode . responseBody <$> httpLbs req man

tokenise :: StrictByteString -> StrictByteString -> [StrictByteString]
tokenise x y =
    let (h, t) = breakSubstring x y
     in h : if BSS.null t then [] else tokenise x (BSS.drop (BSS.length x) t)

parseCookie :: StrictByteString -> (StrictByteString, StrictByteString)
parseCookie s = case tokenise "=" s of
    (k:rest) -> (k, BSS.intercalate "=" rest)
    []       -> (s, "")

parseCookies :: StrictByteString -> [(StrictByteString, StrictByteString)]
parseCookies = map parseCookie . tokenise "; "

checkSessionCookie :: Request -> Maybe StrictByteString
checkSessionCookie = lookup "Cookie" . requestHeaders >=> lookup "session" . parseCookies

verifyStateCookie :: Request -> Bool
verifyStateCookie req = isJust do
    stateParam <- join $ "state" `lookup` queryString req
    stateCookie <- lookup "Cookie" (requestHeaders req) >>= lookup "state" . parseCookies
    if stateCookie == stateParam then Just () else Nothing

data AccessTokenResponse = AccessTokenResponse
    { access_token :: StrictByteString
    , expires_in :: Int
    , credentials_id :: Int
    }

instance FromJSON AccessTokenResponse where
    parseJSON (Object o) = AccessTokenResponse . C8.pack
        <$> o .: "access_token"
        <*> o .: "expires_in"
        <*> o .: "credentials_id"
    parseJSON invalid = typeMismatch "Object" invalid

redirectUri :: EnvCredentials -> C8.ByteString
redirectUri creds = C8.pack creds.host <> "/callback"

requestAuthToken :: Manager -> Discovery -> EnvCredentials -> StrictByteString -> IO (Maybe AccessTokenResponse)
requestAuthToken man disc creds code = do
    req <-  urlEncodedBody [("grant_type", "authorization_code"), ("code", code), ("redirect_uri", redirectUri creds)]
          . applyBasicAuth creds.clientId creds.clientSecret
        <$> parseRequest (C8.unpack disc.token_endpoint)
    decode . responseBody <$> httpLbs req man

addSessionHeader :: AccessTokenResponse -> ResponseHeaders -> ResponseHeaders
addSessionHeader atr = (("Set-Cookie", "session=" <> C8.pack (show atr.credentials_id) <> "; HttpOnly") :)

destroySessionHeader :: Request -> Maybe Header
destroySessionHeader req = (\session -> ("Set-Cookie", "session=" <> session <> "; HttpOnly; Max-Age=-1")) <$> checkSessionCookie req

requireSession :: Manager -> Discovery -> EnvCredentials -> Middleware
requireSession man disc creds app req res
    | pathInfo req == ["logout"] = res $ responseLBS status200 ([("Location", "/")] <> maybeToList (destroySessionHeader req)) ""
    | pathInfo req == ["callback"] && verifyStateCookie req = case join $ "code" `lookup` queryString req of
        Just code -> requestAuthToken man disc creds code >>= \case
            Just atr -> res $ responseLBS status302 (addSessionHeader atr [("Location", "/")]) ""
            Nothing -> res $ responseLBS status500 [] "Could not parse access token response from koala"
        Nothing -> res $ responseLBS status500 [] "Invalid parameters to oauth callback url"
    | pathInfo req == ["callback"] = res $ responseLBS status403 [] "Invalid state parameter"
    | isNothing (checkSessionCookie req) = do
        state <- C8.pack <$> replicateM 10 (getStdRandom (randomR ('a', 'z')))
        let redirectUrl = disc.authorization_endpoint
                <> "?response_type=code"
                <> "&client_id=" <> creds.clientId
                <> "&redirect_uri=" <> redirectUri creds
                <> "&scope=openid%20member-read%20email%20profile"
                <> "&state=" <> state
        res $ responseLBS status302 [("Location", redirectUrl), ("Set-Cookie", "state=" <> state <> "; HttpOnly")] ""
    | otherwise = app req res