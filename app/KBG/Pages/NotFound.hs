{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use catMaybes" #-}

module KBG.Pages.NotFound (notFoundPage) where

import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import Prelude hiding (lookup)
import KBG.Layout

notFoundPage :: Markup
notFoundPage = layout $ do
    H.h1 "404 - Not Found"
    H.p "Have you tried turning it off and on again?"
