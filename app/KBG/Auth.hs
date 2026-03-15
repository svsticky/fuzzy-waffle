{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module KBG.Auth (requireSession, discovery, Discovery) where

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

data Discovery = Discovery
    { authorization_endpoint :: StrictByteString
    , token_endpoint :: StrictByteString
    }

instance FromJSON Discovery where
    parseJSON (Object o) = (\a b -> Discovery (C8.pack a) (C8.pack b))
        <$> o .: "authorization_endpoint"
        <*> o .: "token_endpoint"
    parseJSON invalid = typeMismatch "Object" invalid

discovery :: Manager -> IO Discovery
discovery man = do
    req <- parseRequest "https://koala.dev.svsticky.nl/.well-known/openid-configuration"
    fromJust . decode . responseBody <$> httpLbs req man

tokenise :: StrictByteString -> StrictByteString -> [StrictByteString]
tokenise x y =
    let (h, t) = breakSubstring x y
     in h : if BSS.null t then [] else tokenise x (BSS.drop (BSS.length x) t)

parseCookie :: StrictByteString -> (StrictByteString, StrictByteString)
parseCookie cookieString = let [key, val] = tokenise "=" cookieString in (key, val)

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

requestAuthToken :: Manager -> Discovery -> StrictByteString -> IO (Maybe AccessTokenResponse)
requestAuthToken man disc code = do 
    req <-  urlEncodedBody [("grant_type", "authorization_code"), ("code", code), ("redirect_uri", "http://localhost:3000/callback")]
          . applyBasicAuth "e1aacf592e92eedf71dea19f8c40b5ea5d51ebe3e30b29b843cfc05cb91aadd6" "86b191438b892b9cac2e8d3e4db5f96a711628c49704366e78d3ebde98c28380b"
        <$> parseRequest (C8.unpack disc.token_endpoint)
    decode . responseBody <$> httpLbs req man

addSessionHeader :: StrictByteString -> ResponseHeaders -> ResponseHeaders
addSessionHeader code = (("Set-Cookie", "session=" <> code <> "; HttpOnly") :)

destroySessionHeader :: Request -> Maybe Header
destroySessionHeader req = (\session -> ("Set-Cookie", "session=" <> session <> "; HttpOnly; Max-Age=-1")) <$> checkSessionCookie req

requireSession :: Manager -> Discovery -> Middleware
requireSession man disc app req res
    | pathInfo req == ["logout"] = res $ responseLBS status200 ([("Location", "/")] <> maybeToList (destroySessionHeader req)) ""
    | pathInfo req == ["callback"] && verifyStateCookie req = case join $ "code" `lookup` queryString req of
        Just code -> requestAuthToken man disc code >>= \case
            Just atr -> app req (res . mapResponseHeaders (addSessionHeader atr.access_token))
            Nothing -> res $ responseLBS status500 [] "Could not parse access token response from koala"
        Nothing -> res $ responseLBS status500 [] "Invalid parameters to oauth callback url"
    | pathInfo req == ["callback"] = res $ responseLBS status403 [] "Invalid state parameter"
    | isNothing (checkSessionCookie req) = do
        state <- C8.pack <$> replicateM 10 (getStdRandom (randomR ('a', 'z')))
        let redirectUrl = disc.authorization_endpoint
                <> "?response_type=code"
                <> "&client_id=e1aacf592e92eedf71dea19f8c40b5ea5d51ebe3e30b29b843cfc05cb91aadd6"
                <> "&redirect_uri=http://localhost:3000/callback"
                <> "&scope=openid%20member-read%20email%20profile"
                <> "&state=" <> state
        res $ responseLBS status302 [("Location", redirectUrl), ("Set-Cookie", "state=" <> state <> "; HttpOnly")] ""
    | otherwise = app req res