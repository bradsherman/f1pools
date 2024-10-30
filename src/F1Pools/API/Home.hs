{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module F1Pools.API.Home (
    homePage,
    HomePage (..),
) where

import Data.Aeson (ToJSON)
import F1Pools.Pages (rootF1Page_)
import GHC.Generics (Generic)
import Lucid (
    ToHtml,
    a_,
    div_,
    href_,
    toHtml,
    toHtmlRaw,
 )

homePage :: IO HomePage
homePage = pure HomePage

data HomePage = HomePage
    deriving (Show, Generic, ToJSON)

instance ToHtml HomePage where
    toHtml _ = do
        rootF1Page_ . div_ $
            a_ [href_ "/drivers"] "Drivers"
    toHtmlRaw = toHtml
