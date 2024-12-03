{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module F1Pools.HTML.Race (
    NewRace (..),
    RacePage (..),
) where

import Data.Text (Text)
import Data.Time (UTCTime)
import F1Pools.DB.Race (Race, Race' (..), RaceId, RaceId' (RaceId))
import F1Pools.DB.Season (Season, Season' (seasonDescription, seasonId), SeasonId)
import F1Pools.HTML.Season ()
import F1Pools.HTML.Utils (parseNonEmptyText)
import F1Pools.Pages (rootF1Page_)
import Htmx.Lucid.Core (OnEvent (DomOnEvent), hxOn_, hxPost_, hxTarget_)
import Lucid (
    HtmlT,
    ToHtml,
    button_,
    class_,
    div_,
    for_,
    form_,
    h3_,
    id_,
    input_,
    label_,
    name_,
    option_,
    select_,
    table_,
    td_,
    th_,
    toHtml,
    toHtmlRaw,
    tr_,
    type_,
    value_,
 )
import Web.FormUrlEncoded (FromForm, fromForm, parseUnique)
import Web.Internal.HttpApiData (showt)

data RacePage = RacePage
    { races :: [Race]
    , seasons :: [Season]
    }
    deriving (Show)

instance ToHtml RacePage where
    toHtml page = do
        rootF1Page_ . div_ $ do
            div_ [id_ "races-table"] $ toHtml page.races
            h3_ [class_ "font-bold text-2xl mt-4 mb-4"] "Add A Race"
            form_
                [ class_ "grid grid-cols-5 gap-4"
                , hxTarget_ "#races-table"
                , hxOn_ (DomOnEvent ":after-request") "this.reset()"
                , hxPost_ "/races/new"
                ]
                $ do
                    div_ [class_ "grid flex-col"] $ do
                        label_ [for_ "season"] "Season"
                        select_ [id_ "season", name_ "season"] $ foldMap seasonOption page.seasons
                    div_ [class_ "grid flex-col"] $ do
                        label_ [for_ "name"] "Race Name"
                        input_ [type_ "text", id_ "name", name_ "name", class_ "border rounded-md p-2"]
                    div_ [class_ "grid flex-col"] $ do
                        label_ [for_ "location"] "Location"
                        input_ [type_ "text", id_ "location", name_ "location", class_ "border rounded-md p-2"]
                    div_ [class_ "grid flex-col"] $ do
                        label_ [for_ "start_time"] "Start Time"
                        input_ [type_ "text", id_ "start_time", name_ "start_time", class_ "border rounded-md p-2"]
                    button_ [class_ "w-1/2 h-1/2 rounded-full self-end bg-green-400"] "Add Race"
      where
        seasonOption :: (Monad m) => Season -> HtmlT m ()
        seasonOption season = option_ [value_ (showt season.seasonId)] $ toHtml season.seasonDescription

    toHtmlRaw = toHtml

data NewRace = NewRace
    { season :: SeasonId
    , name :: Text
    , location :: Text
    , startTime :: UTCTime
    }

instance FromForm NewRace where
    fromForm f =
        NewRace
            <$> parseUnique "season" f
            <*> parseNonEmptyText "name" f
            <*> parseNonEmptyText "location" f
            <*> parseUnique "start_time" f

instance ToHtml RaceId where
    toHtml (RaceId rId) = toHtml $ show rId
    toHtmlRaw = toHtml

instance ToHtml Race where
    toHtml race =
        tr_ [class_ "border grid grid-cols-5 gap-4"] $ do
            td_ [class_ "p-2 content-center"] (toHtml race.raceSeasonId)
            td_ [class_ "p-2 content-center"] (toHtml race.raceName)
            td_ [class_ "p-2 content-center"] (toHtml race.raceLocation)
            td_ [class_ "p-2 content-center"] (toHtml $ show race.startTime)
            td_ [class_ "p-2 content-center"] ""
    toHtmlRaw = toHtml

instance ToHtml [Race] where
    toHtml races = do
        h3_ [class_ "font-bold text-2xl mt-4 mb-4"] "Races"
        table_ [class_ "table-auto mt-3 mb-3 w-full"] $ do
            tr_ [class_ "grid grid-cols-5 gap-4"] $ do
                th_ [class_ "p-2 justify-self-start"] "Season"
                th_ [class_ "p-2 justify-self-start"] "Name"
                th_ [class_ "p-2 justify-self-start"] "Location"
                th_ [class_ "p-2 justify-self-start"] "Time"
                th_ [class_ "p-2 justify-self-start"] ""
            foldMap toHtml races
    toHtmlRaw = toHtml
