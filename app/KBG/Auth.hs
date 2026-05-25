{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module KBG.Auth (requireSession, discovery, Discovery, EnvCredentials(..), loadEnvCredentials, parseCookies, SessionStore) where

import Network.Wai
import Network.HTTP.Client (responseBody, Manager, httpLbs, parseRequest, applyBasicAuth, urlEncodedBody)
import Network.HTTP.Types
import Data.Maybe (fromJust, isNothing, isJust, maybeToList, fromMaybe)

import Data.ByteString (StrictByteString, breakSubstring)
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Char8 as C8

import Data.Aeson.Types (typeMismatch)
import Data.Aeson
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Network.HTTP.Client as HC
import Control.Monad
import System.Random

import KBG.Config (EnvCredentials(..), loadEnvCredentials)
import Text.Read (readMaybe)

data Discovery = Discovery
    { authorization_endpoint :: StrictByteString
    , token_endpoint         :: StrictByteString
    , userinfo_endpoint      :: StrictByteString
    }

instance FromJSON Discovery where
    parseJSON (Object o) = (\a b c -> Discovery (C8.pack a) (C8.pack b) (C8.pack c))
        <$> o .: "authorization_endpoint"
        <*> o .: "token_endpoint"
        <*> o .: "userinfo_endpoint"
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

checkSessionCookie :: Request -> Maybe (Int, Bool)
checkSessionCookie req = do
    cookie <- lookup "Cookie" (requestHeaders req)
    val    <- lookup "session" (parseCookies cookie)
    case C8.split ':' val of
        [uid, adm] -> (,) <$> readMaybe (C8.unpack uid) <*> pure (adm == "1")
        _          -> Nothing

verifyStateCookie :: Request -> Bool
verifyStateCookie req = isJust do
    stateParam <- join $ "state" `lookup` queryString req
    stateCookie <- lookup "Cookie" (requestHeaders req) >>= lookup "state" . parseCookies
    if stateCookie == stateParam then Just () else Nothing

data AccessTokenResponse = AccessTokenResponse
    { access_token   :: StrictByteString
    , expires_in     :: Int
    , credentials_id :: Int
    }

instance FromJSON AccessTokenResponse where
    parseJSON (Object o) = AccessTokenResponse . C8.pack
        <$> o .: "access_token"
        <*> o .: "expires_in"
        <*> o .: "credentials_id"
    parseJSON invalid = typeMismatch "Object" invalid

redirectUri :: EnvCredentials -> C8.ByteString
redirectUri creds = C8.pack creds.host <> ":" <> C8.pack (show creds.port) <> "/callback"

requestAuthToken :: Manager -> Discovery -> EnvCredentials -> StrictByteString -> IO (Maybe AccessTokenResponse)
requestAuthToken man disc creds code = do
    req <-  urlEncodedBody [("grant_type", "authorization_code"), ("code", code), ("redirect_uri", redirectUri creds)]
          . applyBasicAuth creds.clientId creds.clientSecret
        <$> parseRequest (C8.unpack disc.token_endpoint)
    decode . responseBody <$> httpLbs req man

newtype UserClaims = UserClaims { isAdmin :: Bool }

instance FromJSON UserClaims where
    parseJSON (Object o) = UserClaims <$> o .: "is_admin"
    parseJSON invalid = typeMismatch "Object" invalid

type SessionStore = IORef (Map Int Bool)

fetchUserClaims :: Manager -> Discovery -> AccessTokenResponse -> IO (Maybe UserClaims)
fetchUserClaims man disc atr = do
    req <- parseRequest (C8.unpack disc.userinfo_endpoint)
    let req' = req { HC.requestHeaders = [("Authorization", "Bearer " <> access_token atr)] }
    decode . responseBody <$> httpLbs req' man

addSessionHeader :: AccessTokenResponse -> Bool -> ResponseHeaders -> ResponseHeaders
addSessionHeader atr isAdm = (("Set-Cookie", "session=" <> C8.pack (show atr.credentials_id) <> ":" <> (if isAdm then "1" else "0") <> "; HttpOnly") :)

destroySessionHeader :: Request -> Maybe Header
destroySessionHeader req = do
    cookie <- lookup "Cookie" (requestHeaders req)
    val    <- lookup "session" (parseCookies cookie)
    return ("Set-Cookie", "session=" <> val <> "; HttpOnly; Max-Age=-1")

requireSession :: Manager -> Discovery -> EnvCredentials -> Middleware
requireSession man disc creds app req res
    | pathInfo req == ["logout"] =
        res $ responseLBS status200 ([("Location", "/")] <> maybeToList (destroySessionHeader req)) ""
    | pathInfo req == ["callback"] && verifyStateCookie req =
        case join $ "code" `lookup` queryString req of
            Just code -> requestAuthToken man disc creds code >>= \case
                Just atr -> fetchUserClaims man disc atr >>= \case
                    Just claims -> do
                        let dest = fromMaybe "/" $ lookup "Cookie" (requestHeaders req)
                                    >>= lookup "redirect" . parseCookies
                        res $ responseLBS status302 (addSessionHeader atr (isAdmin claims) [("Location", dest)]) ""                    
                    Nothing ->
                        res $ responseLBS status500 [] "Could not fetch user claims"
                Nothing ->
                    res $ responseLBS status500 [] "Could not parse access token response from koala"
            Nothing ->
                res $ responseLBS status500 [] "Invalid parameters to oauth callback url"
    | pathInfo req == ["callback"] =
        res $ responseLBS status403 [] "Invalid state parameter"
    | isNothing (checkSessionCookie req) = do
        state <- C8.pack <$> replicateM 10 (getStdRandom (randomR ('a', 'z')))
        let currentPath = rawPathInfo req
            redirectUrl = disc.authorization_endpoint
                <> "?response_type=code"
                <> "&client_id=" <> creds.clientId
                <> "&redirect_uri=" <> redirectUri creds
                <> "&scope=openid%20member-read%20email%20profile"
                <> "&state=" <> state
        res $ responseLBS status302
            [ ("Location", redirectUrl)
            , ("Set-Cookie", "state=" <> state <> "; HttpOnly")
            , ("Set-Cookie", "redirect=" <> currentPath <> "; HttpOnly")
            ] ""
    | otherwise = app req res