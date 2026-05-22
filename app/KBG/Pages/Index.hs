{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use catMaybes" #-}

module KBG.Pages.Index (mainPage, renderField, formFields) where

import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import Data.Text (Text)
import qualified Data.Text as T
import Text.Blaze.Html5.Attributes as HA
import Prelude hiding (lookup)
import KBG.Layout
import Data.List

formFields :: [(Text, Text, Text)]
formFields =
    [ ("voo",   "Voorzitter",     "Ruben Broere")
    , ("sec",   "Secretaris",     "Aron den Ouden")
    , ("pen",   "Penningmeester", "Thijs Olijerhoek")
    , ("int",   "Intern",         "Toine Ruinard")
    , ("ext",   "Extern",         "Egon Ruiter")
    , ("ond",   "Onderwijs",      "Thom Bongaards")
    ]

mainPage :: [(String, String)] -> Maybe String -> Markup
mainPage prefilled banner = layout $ do
    case banner of
        Nothing  -> mempty
        Just msg -> H.div ! HA.class_ "banner" $ H.toHtml msg
    H.form ! HA.action "/" ! HA.method "post" ! HA.id "mainForm" $ do
        mapM_ (renderField prefilled) formFields
        H.input ! HA.type_ "submit" ! HA.value "Opslaan"
        validationScript

renderField :: [(String, String)] -> (Text, Text, Text) -> Markup
renderField prefilled (fieldId, label_, placeholder_) =
    H.div ! HA.class_ "field" $ do
        H.label ! HA.for (textValue fieldId) $ H.toHtml label_
        let val = maybe "" T.pack (Data.List.lookup (T.unpack fieldId) prefilled)
        H.input ! HA.type_ "text"
                ! HA.id    (textValue fieldId)
                ! HA.name  (textValue fieldId)
                ! HA.placeholder (textValue placeholder_)
                ! HA.autocomplete "off"
                ! HA.value (textValue val)
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
