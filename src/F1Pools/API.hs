{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module F1Pools.API (
    f1poolsApp,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy (Proxy))
import Database.PostgreSQL.Simple (Connection)
import F1Pools.API.Driver (DriverPage, NewDriver, driverPage, handleNewDriver)
import F1Pools.DB (
    Race,
    Race' (..),
    RaceId,
    RaceId' (RaceId),
    Season,
    SeasonId,
    SeasonId' (SeasonId),
    raceSelect,
    runRaceSelect,
    runSeasonSelect,
    seasonId,
    seasonSelect,
 )
import Lucid (ToHtml, table_, td_, th_, toHtml, toHtmlRaw, tr_)
import Network.Wai (Application)
import Servant (FormUrlEncoded, Header, Headers, NoContent, ReqBody, Server, serve, (:<|>) (..))
import Servant.API (Get, JSON, StdMethod (POST), Verb, (:>))
import Servant.HTML.Lucid (HTML)

f1poolsApp :: Connection -> Application
f1poolsApp conn = serve f1poolsAPI (server1 conn)

f1poolsAPI :: Proxy F1poolsAPI
f1poolsAPI = Proxy

type F1poolsAPI =
    "seasons" :> Get '[JSON, HTML] [Season]
        :<|> "races" :> Get '[JSON, HTML] [Race]
        :<|> "drivers" :> Get '[JSON, HTML] DriverPage
        :<|> "driver"
            :> "new"
            :> ReqBody '[FormUrlEncoded] NewDriver
            :> Verb
                'POST
                301
                '[HTML]
                (Headers '[Header "Location" String] NoContent)

server1 :: Connection -> Server F1poolsAPI
server1 conn =
    liftIO (runSeasonSelect conn seasonSelect)
        :<|> liftIO (runRaceSelect conn raceSelect)
        :<|> liftIO (driverPage conn)
        :<|> handleNewDriver conn

instance ToHtml SeasonId where
    toHtml (SeasonId sId) = toHtml $ show sId
    toHtmlRaw = toHtml

instance ToHtml Season where
    toHtml season =
        tr_ $ do
            td_ (toHtml $ seasonId season)
    toHtmlRaw = toHtml

instance ToHtml [Season] where
    toHtml seasons = table_ $ do
        tr_ $ do
            th_ "Season #"
        foldMap toHtml seasons
    toHtmlRaw = toHtml

instance ToHtml RaceId where
    toHtml (RaceId rId) = toHtml $ show rId
    toHtmlRaw = toHtml

instance ToHtml Race where
    toHtml race =
        tr_ $ do
            td_ (toHtml $ raceSeasonId race)
            td_ (toHtml $ raceName race)
            td_ (toHtml $ raceLocation race)
            td_ (toHtml . show $ startTime race)
    toHtmlRaw = toHtml

instance ToHtml [Race] where
    toHtml seasons = table_ $ do
        tr_ $ do
            th_ "Season #"
            th_ "Name"
            th_ "Location"
            th_ "Time"
        foldMap toHtml seasons
    toHtmlRaw = toHtml
