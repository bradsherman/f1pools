{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module F1Pools.HTML.Season (

) where

import F1Pools.DB.Season (Season, Season' (seasonId), SeasonId, SeasonId' (SeasonId))
import F1Pools.Pages (rootF1Page_)
import Lucid (ToHtml, table_, td_, th_, toHtml, toHtmlRaw, tr_)

instance ToHtml SeasonId where
    toHtml (SeasonId sId) = toHtml $ show sId
    toHtmlRaw = toHtml

instance ToHtml Season where
    toHtml season =
        tr_ $ do
            td_ (toHtml $ seasonId season)
    toHtmlRaw = toHtml

instance ToHtml [Season] where
    toHtml seasons = rootF1Page_ . table_ $ do
        tr_ $ do
            th_ "Season #"
        foldMap toHtml seasons
    toHtmlRaw = toHtml
