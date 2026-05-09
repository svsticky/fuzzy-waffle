{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use catMaybes" #-}
{-# LANGUAGE OverloadedRecordDot #-}

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
import qualified Data.ByteString.Char8 as C8

import KBG.Auth ( discovery, requireSession, loadEnvCredentials, EnvCredentials(..) )
import KBG.Styles ( layout )

-- | (field id/name, label text, placeholder)
formFields :: [(Text, Text, Text)]
formFields =
    [ ("voo",   "Voorzitter",     "Iris van der Zwart")
    , ("sec",   "Secretaris",     "Bram de Haas")
    , ("pen",   "Penningmeester", "Chion Craane")
    , ("int",   "Intern",         "Rens van Moorsel")
    , ("ext",   "Extern",         "Isabelle Wittebols")
    , ("ond",   "Onderwijs",      "Jari van Polen")
    ]

page :: Markup
page = layout $
    H.form ! HA.action "/" ! HA.method "post" ! HA.id "mainForm" $ do
        mapM_ renderField formFields
        H.input ! HA.type_ "submit" ! HA.value "Submit"
        validationScript

renderField :: (Text, Text, Text) -> Markup
renderField (fieldId, label_, placeholder_) =
    H.div ! HA.class_ "field" $ do
        H.label ! HA.for (textValue fieldId) $ H.toHtml label_
        H.input ! HA.type_ "text"
                ! HA.id    (textValue fieldId)
                ! HA.name  (textValue fieldId)
                ! HA.placeholder (textValue placeholder_)
                ! HA.autocomplete "off"
        H.span  ! HA.id (textValue (fieldId <> "-error"))
                ! HA.class_ "error-msg" $ ""

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
    envCreds <- loadEnvCredentials
    let settings = setPort envCreds.port defaultSettings
    putStrLn $ "listening on " ++ C8.unpack envCreds.baseURL
    storedData <- (decode <$> BSL.readFile "./data.json") :: IO (Maybe [Bet])
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
htmlResponse status = responseLBS status [("Content-Type", "text/html; charset=utf-8")]

lookupParam :: Text -> [(BS.ByteString, BS.ByteString)] -> Maybe Text
lookupParam key params = decodeUtf8 <$> lookup (encodeUtf8_ key) params
  where encodeUtf8_ = Data.Text.Encoding.encodeUtf8

notFoundRoute :: W.Response
notFoundRoute = responseLBS status404 [("Content-Type", "text/html")]
    "404 - Not Found<br>Have you tried turning it off and on again?"