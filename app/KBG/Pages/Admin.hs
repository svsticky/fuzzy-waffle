{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module KBG.Pages.Admin where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Blaze.Html (Markup, toHtml, (!))
import KBG.DB (Submission(..))

adminPage :: [Submission] -> Markup
adminPage subs = do
    H.h1 "Admin"
    if null subs
        then H.p "No submissions yet."
        else H.table ! HA.class_ "admin-table" $ do
            H.thead $ H.tr $ do
                H.th "User ID"
                H.th "Voorzitter"
                H.th "Secretaris"
                H.th "Penningmeester"
                H.th "Intern"
                H.th "Extern"
                H.th "Onderwijs"
                H.th "Submitted At"

            H.tbody $
                mapM_ row subs
  where
    row :: Submission -> Markup
    row s = H.tr $ do
        H.td $ toHtml (show s.subUserId)
        H.td $ toHtml s.subVoo
        H.td $ toHtml s.subSec
        H.td $ toHtml s.subPen
        H.td $ toHtml s.subInt
        H.td $ toHtml s.subExt
        H.td $ toHtml s.subOnd
        H.td $ toHtml (show s.subTime)