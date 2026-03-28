{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Network.Wai as W
import Network.Wai.Handler.Warp
import Network.Wai.Parse
import Network.HTTP.Types
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import GHC.Generics (Generic)
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Network.HTTP.Client.TLS
import Data.Aeson
import Text.Blaze.Html5.Attributes as HA
import Text.Blaze.Html.Renderer.Utf8

import KBG.Auth

layout :: Markup -> Markup
layout body = H.html $ do
    H.head $ do
        H.title (text "My website!")
    H.body body -- add nav bar

page :: Markup
page = layout $ do
    H.form ! HA.action "/" ! HA.method "post" $ do
        H.label ! HA.for "onderwijs" $ "Onderwijs: "
        H.input ! HA.type_ "text"
                ! HA.id "onderwijs"
                ! HA.name "onderwijs"
                ! HA.placeholder "Jari van Polen"
        H.br

        H.label ! HA.for "intern" $ "Intern: "
        H.input ! HA.type_ "text"
                ! HA.id "intern"
                ! HA.name "intern"
                ! HA.placeholder "Rens van Moorsel"
        H.br

        H.label ! HA.for "secretaris" $ "Secretaris: "
        H.input ! HA.type_ "text"
                ! HA.id "secretaris"
                ! HA.name "secretaris"
                ! HA.placeholder "Bram de Haas"
        H.br

        H.label ! HA.for "voorzitter" $ "Voorzitter: "
        H.input ! HA.type_ "text"
                ! HA.id "voorzitter"
                ! HA.name "voorzitter"
                ! HA.placeholder "Iris van der Zwart"
        H.br

        H.label ! HA.for "penningmeester" $ "Penningmeester: "
        H.input ! HA.type_ "text"
                ! HA.id "penningmeester"
                ! HA.name "penningmeester"
                ! HA.placeholder "Chion Craane"
        H.br

        H.label ! HA.for "extern" $ "Extern: "
        H.input ! HA.type_ "text"
                ! HA.id "extern"
                ! HA.name "extern"
                ! HA.placeholder "Isabelle Wittebols"
        H.br

        H.input ! HA.type_ "submit" ! HA.value "Submit"

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
    let host_ = "0.0.0.0"
    let port_ = 3000
    let settings = setPort port_ $ setHost host_ defaultSettings
    putStrLn $ "listening on " ++ show host_ ++ ":" ++ show port_
    contents <- (decode <$> BSL.readFile "./data.json") :: IO (Maybe [Bet])
    -- print contents
    manager <- newTlsManager
    disc <- discovery manager
    runSettings settings (requireSession manager disc app)

app :: Application
app req res = do
    case requestMethod req of
        "GET" -> res $ responseLBS status200 [("Content-Type", "text/html")] (renderHtml page)
        "POST" -> do
            (params, _files) <- parseRequestBody lbsBackEnd req
            let onderwijs = lookupParam "onderwijs" params
                intern = lookupParam "intern" params
                secretaris = lookupParam "secretaris" params
                voorzitter = lookupParam "voorzitter" params
                penningmeester = lookupParam "penningmeester" params
                extern = lookupParam "extern" params
                inp = [onderwijs, intern, secretaris, voorzitter, penningmeester, extern]
            let response = (if Prelude.any (\s -> s == Just "\"\"") inp then "No input provided" else "Found, " <> show inp)
            res $ responseLBS status200 [("Content-Type", "text/html")] (renderHtml $ H.p $ H.toHtml response)
        _ -> res $ responseLBS status405 [("Content-Type", "text/plain")] "Method not allowed"


lookupParam :: BS.ByteString -> [(BS.ByteString, BS.ByteString)] -> Maybe String
lookupParam key params = fmap (show . decodeUtf8) (lookup key params)

notFoundRoute :: W.Response
notFoundRoute =
    responseLBS
    status404
    [("Content-Type", "text/html")]
    "404 - Not Found <br>Have you tried turning it off and on again?"