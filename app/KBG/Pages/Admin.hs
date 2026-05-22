{-# LANGUAGE OverloadedStrings #-}

module KBG.Pages.Admin where

import KBG.Layout
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html (Markup)

adminPage :: Markup
adminPage = layout $ do
    H.h1 "Admin"
    H.p "So do you feel privileged now?"
