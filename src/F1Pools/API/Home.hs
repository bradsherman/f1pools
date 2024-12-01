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
    class_,
    div_,
    href_,
    p_,
    toHtml,
    toHtmlRaw,
 )

homePage :: IO HomePage
homePage = pure HomePage

data HomePage = HomePage
    deriving (Show, Generic, ToJSON)

instance ToHtml HomePage where
    toHtml _ = do
        rootF1Page_ . div_ $ do
            p_ [class_ "text-lg"] "Welcome to F1 pools!"
            div_ $ a_ [href_ "/drivers"] "Drivers"
            div_ $ a_ [href_ "/racers"] "Racers"
            div_ $ a_ [href_ "/seasons"] "Seasons"

    toHtmlRaw = toHtml
