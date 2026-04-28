{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use catMaybes" #-}

module Main where

import Network.Wai as W
import Network.Wai.Handler.Warp
import Network.Wai.Parse
import Network.HTTP.Types
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Network.HTTP.Client.TLS
import Data.Aeson
import Text.Blaze.Html5.Attributes as HA
import Text.Blaze.Html.Renderer.Utf8

import KBG.Auth

-- | (field id/name, label text, placeholder)
formFields :: [(Text, Text, Text)]
formFields =
    [ ("ond",   "Onderwijs",     "Jari van Polen")
    , ("int",   "Intern",        "Rens van Moorsel")
    , ("sec",   "Secretaris",    "Bram de Haas")
    , ("voo",   "Voorzitter",    "Iris van der Zwart")
    , ("pen",   "Penningmeester","Chion Craane")
    , ("ext",   "Extern",        "Isabelle Wittebols")
    ]

layout :: Markup -> Markup
layout body = H.html $ do
    H.head $ do
        H.title "KandidaatsBestuurGokken"
        H.style $ H.toHtml ("input.error { border: 2px solid red; } \
                             \.error-msg { color: red; font-size: 0.85em; }" :: Text)
    H.body body

page :: Markup
page = layout $ do
    H.form ! HA.action "/" ! HA.method "post" ! HA.id "mainForm" $ do
        mapM_ renderField formFields
        H.input ! HA.type_ "submit" ! HA.value "Submit"
    validationScript

renderField :: (Text, Text, Text) -> Markup
renderField (fieldId, label, placeholder) = do
    H.label ! HA.for (textValue fieldId) $ H.toHtml (label <> ": ")
    H.input ! HA.type_ "text"
            ! HA.id    (textValue fieldId)
            ! HA.name  (textValue fieldId)
            ! HA.placeholder (textValue placeholder)
    H.span  ! HA.id (textValue (fieldId <> "-error"))
            ! HA.class_ "error-msg" $ ""
    H.br

validationScript :: Markup
validationScript = H.script $ H.toHtml $ T.unlines
    [ "document.getElementById('mainForm').addEventListener('submit', function(e) {"
    , "  var fields = " <> fieldList <> ";"
    , "  var ok = true;"
    , "  fields.forEach(function(id) {"
    , "    var el = document.getElementById(id);"
    , "    var err = document.getElementById(id + '-error');"
    , "    if (!el.value.trim()) {"
    , "      el.classList.add('error');"
    , "      err.textContent = ' This field is required.';"
    , "      ok = false;"
    , "    }"
    , "    el.addEventListener('input', function() {"
    , "      el.classList.remove('error');"
    , "      err.textContent = '';"
    , "    }, { once: true });"
    , "  });"
    , "  if (!ok) e.preventDefault();"
    , "});"
    ]
  where
    fieldList = "['" <> T.intercalate "','" (map (\(f,_,_) -> f) formFields) <> "']"

data Bettee = Bettee
    { name     :: String
    , function :: String
    } deriving (Show, Generic, FromJSON)

data Bet = Bet
    { better  :: String
    , bettees :: [Bettee]
    } deriving (Show, Generic, FromJSON)

main :: IO ()
main = do
    let host_ = "0.0.0.0"
    let port_ = 3000
    let settings = setPort port_ $ setHost host_ defaultSettings
    putStrLn $ "listening on " ++ show host_ ++ ":" ++ show port_
    contents <- (decode <$> BSL.readFile "./data.json") :: IO (Maybe [Bet])
    print contents
    manager <- newTlsManager
    disc <- discovery manager
    runSettings settings (requireSession manager disc app)

app :: Application
app req res = case requestMethod req of
    "GET"  -> res $ htmlResponse status200 (renderHtml page)
    "POST" -> do
        (params, _) <- parseRequestBody lbsBackEnd req
        let values = map (\(f,_,_) -> lookupParam f params) formFields
        let response = T.intercalate ", " [v | Just v <- values]
        res $ htmlResponse status200 (renderHtml . H.p $ H.toHtml response)
    _ -> res $ responseLBS status405 [("Content-Type", "text/plain")] "Method not allowed"

htmlResponse :: Status -> BSL.ByteString -> W.Response
htmlResponse status = responseLBS status [("Content-Type", "text/html")]

lookupParam :: Text -> [(BS.ByteString, BS.ByteString)] -> Maybe Text
lookupParam key params = decodeUtf8 <$> lookup (encodeUtf8 key) params
  where encodeUtf8 = Data.Text.Encoding.encodeUtf8

notFoundRoute :: W.Response
notFoundRoute = responseLBS status404 [("Content-Type", "text/html")]
    "404 - Not Found<br>Have you tried turning it off and on again?"