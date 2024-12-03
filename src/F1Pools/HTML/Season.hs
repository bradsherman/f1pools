{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module F1Pools.HTML.Season (
    SeasonPage (..),
    NewSeason (..),
) where

import Data.Text (Text)
import F1Pools.DB.Season (
    Season,
    Season' (seasonDescription, seasonId),
    SeasonId,
    SeasonId' (SeasonId),
 )
import F1Pools.HTML.Utils (parseNonEmptyText)
import F1Pools.Pages (rootF1Page_)
import Htmx.Lucid.Core (OnEvent (DomOnEvent), hxOn_, hxPost_, hxTarget_)
import Lucid (
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
    table_,
    td_,
    th_,
    toHtml,
    toHtmlRaw,
    tr_,
    type_,
 )
import Web.FormUrlEncoded (FromForm, fromForm)

newtype SeasonPage = SeasonPage
    { seasons :: [Season]
    }
    deriving (Show)

instance ToHtml SeasonPage where
    toHtml page = do
        rootF1Page_ . div_ $ do
            div_ [id_ "seasons-table"] . toHtml $ seasons page
            h3_ [class_ "font-bold text-2xl mt-4 mb-4"] "Add A Season"
            form_
                [ class_ "grid grid-cols-4 gap-4"
                , hxTarget_ "#seasons-table"
                , hxOn_ (DomOnEvent ":after-request") "this.reset()"
                , hxPost_ "/seasons/new"
                ]
                $ do
                    div_ [class_ "grid flex-col"] $ do
                        label_ [for_ "description"] "Season Description"
                        input_ [type_ "text", id_ "description", name_ "description", class_ "border rounded-md p-2"]
                    button_ [class_ "w-1/2 h-1/2 rounded-full self-end bg-green-400"] "Add Season"

    toHtmlRaw = toHtml

newtype NewSeason = NewSeason {description :: Text}

instance FromForm NewSeason where
    fromForm f = NewSeason <$> parseNonEmptyText "description" f

instance ToHtml SeasonId where
    toHtml (SeasonId sId) = toHtml $ show sId
    toHtmlRaw = toHtml

instance ToHtml Season where
    toHtml season =
        tr_ [class_ "border grid grid-cols-5 gap-4"] $ do
            td_ [class_ "p-2 content-center"] (toHtml season.seasonId)
            td_ [class_ "p-2 content-center"] (toHtml season.seasonDescription)
    toHtmlRaw = toHtml

instance ToHtml [Season] where
    toHtml drivers = do
        h3_ [class_ "font-bold text-2xl mt-4 mb-4"] "Drivers"
        table_ [class_ "table-auto mt-3 mb-3 w-full"] $ do
            tr_ [class_ "grid grid-cols-3 gap-4"] $ do
                th_ [class_ "p-2 justify-self-start"] "Season"
                th_ [class_ "p-2 justify-self-start"] "Description"
                th_ [class_ "p-2 justify-self-start"] ""
            foldMap toHtml drivers
    toHtmlRaw = toHtml
