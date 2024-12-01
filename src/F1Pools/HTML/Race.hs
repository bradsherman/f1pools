{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module F1Pools.HTML.Race (

) where

import F1Pools.DB.Race (Race, Race' (..), RaceId, RaceId' (RaceId))
import F1Pools.HTML.Season ()
import F1Pools.Pages (rootF1Page_)
import Lucid (ToHtml, table_, td_, th_, toHtml, toHtmlRaw, tr_)

instance ToHtml RaceId where
    toHtml (RaceId rId) = toHtml $ show rId
    toHtmlRaw = toHtml

instance ToHtml Race where
    toHtml race =
        tr_ $ do
            td_ (toHtml race.raceSeasonId)
            td_ (toHtml race.raceName)
            td_ (toHtml race.raceLocation)
            td_ (toHtml $ show race.startTime)
    toHtmlRaw = toHtml

instance ToHtml [Race] where
    toHtml seasons = rootF1Page_ . table_ $ do
        tr_ $ do
            th_ "Season #"
            th_ "Name"
            th_ "Location"
            th_ "Time"
        foldMap toHtml seasons
    toHtmlRaw = toHtml
