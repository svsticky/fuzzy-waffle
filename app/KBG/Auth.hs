{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module KBG.Auth (requireSession, discovery, Discovery, EnvCredentials(..), loadEnvCredentials, parseCookies) where

import Network.Wai
import Network.HTTP.Client (responseBody, Manager, httpLbs, parseRequest, applyBasicAuth, urlEncodedBody)
import Network.HTTP.Types
import Data.Maybe (isNothing, isJust, maybeToList, fromMaybe)

import Data.ByteString (StrictByteString, breakSubstring)
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Char8 as C8

import Data.Aeson.Types (typeMismatch)
import Data.Aeson
import Control.Monad
import System.Random

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Debug.Trace (trace)

import Control.Exception (try)
import System.IO (hPutStrLn, stderr)
import qualified Network.HTTP.Client as HC
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
    let url = envCreds.oAuthReq
    req0 <- parseRequest url
    let req = req0 { HC.responseTimeout = HC.responseTimeoutMicro 10000000 }
    hPutStrLn stderr ("discovery: GET " <> url)
    res <- try (httpLbs req man) >>= \case
        Right r -> pure r
        Left e  -> failWith ("discovery: request to " <> url <> " failed: " <> show (e :: HC.HttpException))
    let code = statusCode (HC.responseStatus res)
        body = responseBody res
    case eitherDecode body of
        Right d | code == 200 -> pure d
        Right _  -> failWith ("discovery: " <> url <> " returned HTTP " <> show code)
        Left err -> failWith ("discovery: " <> url <> " HTTP " <> show code
                        <> ", cannot decode: " <> err
                        <> " | body starts: " <> C8.unpack (BL.toStrict (BL.take 300 body)))
  where
    failWith :: String -> IO a
    failWith msg = hPutStrLn stderr msg >> fail msg

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
    , sid :: StrictByteString
    }

instance FromJSON AccessTokenResponse where
    parseJSON = withObject "AccessTokenResponse" \o -> do
        tok <- o .: "access_token"
        s   <- either fail pure (extractSid tok)
        AccessTokenResponse (TE.encodeUtf8 tok)
            <$> o .: "expires_in"
            <*> pure (TE.encodeUtf8 s)

redirectUri :: EnvCredentials -> C8.ByteString
redirectUri creds = C8.pack creds.host <> "/callback"

extractSid :: T.Text -> Either String T.Text
extractSid jwt = do
  payload <- case T.splitOn "." jwt of
    [_, p, _] -> Right p
    _         -> Left "Invalid JWT: expected 3 dot-separated parts"
  decoded <- B64.decodeUnpadded (TE.encodeUtf8 payload)
  obj     <- A.eitherDecodeStrict decoded
  case KM.lookup "sid" obj of
    Just (A.String s) -> Right s
    Just _            -> Left "sid is not a string"
    Nothing           -> Left "No sid claim in token"

requestAuthToken :: Manager -> Discovery -> EnvCredentials -> StrictByteString -> IO (Either String AccessTokenResponse)
requestAuthToken man disc creds code = do
    req <-  urlEncodedBody [("grant_type", "authorization_code"), ("code", code), ("redirect_uri", redirectUri creds)]
            . applyBasicAuth creds.clientId creds.clientSecret
        <$> parseRequest (C8.unpack disc.token_endpoint)
    eitherDecode . responseBody <$> httpLbs req man

addSessionHeader :: AccessTokenResponse -> ResponseHeaders -> ResponseHeaders
addSessionHeader atr = (("Set-Cookie", "session=" <> atr.sid <> "; HttpOnly; Path=/; SameSite=Lax") :)

destroySessionHeader :: Request -> Maybe Header
destroySessionHeader req = (\session -> ("Set-Cookie", "session=" <> session <> "; HttpOnly; Max-Age=-1")) <$> checkSessionCookie req

requireSession :: Manager -> Discovery -> EnvCredentials -> Middleware
requireSession man disc creds app req res
    | pathInfo req == ["logout"] = res $ responseLBS status200 ([("Location", "/")] <> maybeToList (destroySessionHeader req)) ""
    | pathInfo req == ["callback"] && verifyStateCookie req = case join $ "code" `lookup` queryString req of
        Just code -> requestAuthToken man disc creds code >>= \case
            Right atr -> res $ responseLBS status302 (addSessionHeader atr [("Location", "/")]) ""
            Left err  -> res $ responseLBS status500 [] (BL.fromStrict (C8.pack ("Could not parse token response: " <> err)))
        Nothing -> res $ responseLBS status500 [] "Invalid parameters to oauth callback url"
    | pathInfo req == ["callback"] = res $ responseLBS status403 [] "Invalid state parameter"
    | isNothing (checkSessionCookie req) = do
        state <- C8.pack <$> replicateM 10 (getStdRandom (randomR ('a', 'z')))
        let redirectUrl = disc.authorization_endpoint
                <> "?response_type=code"
                <> "&client_id=" <> creds.clientId
                <> "&redirect_uri=" <> redirectUri creds
                <> "&scope=openid%20email%20profile"
                <> "&state=" <> state
        res $ responseLBS status302 [("Location", redirectUrl), ("Set-Cookie", "state=" <> state <> "; HttpOnly")] ""
    | otherwise = app req res
