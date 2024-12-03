{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}

module F1Pools.API.Season (
    SeasonsAPI (..),
    seasonsHandler,
) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple (Connection)
import F1Pools.DB.Season (Season, Season' (Season), SeasonId' (SeasonId), insertSeason, runSeasonSelect, seasonSelect)
import F1Pools.HTML.Season (NewSeason (description), SeasonPage (SeasonPage))
import GHC.Generics (Generic)
import Opaleye (runInsert, toFields)
import Servant (FormUrlEncoded, Get, Handler, Post, ReqBody, (:-), (:>))
import Servant.HTML.Lucid (HTML)
import Servant.Server.Generic (AsServer)

data SeasonsAPI mode = SeasonsAPI
    { list :: mode :- Get '[HTML] SeasonPage
    , create :: mode :- "new" :> ReqBody '[FormUrlEncoded] NewSeason :> Post '[HTML] [Season]
    }
    deriving stock (Generic)

seasonsHandler :: Connection -> SeasonsAPI AsServer
seasonsHandler conn =
    SeasonsAPI
        { list = listSeasonsHandler conn
        , create = createSeasonHandler conn
        }

listSeasonsHandler :: Connection -> Handler SeasonPage
listSeasonsHandler conn = SeasonPage <$> liftIO (runSeasonSelect conn seasonSelect)

createSeasonHandler :: Connection -> NewSeason -> Handler [Season]
createSeasonHandler conn newSeason = liftIO $ do
    void . runInsert conn $ insertSeason theSeason
    runSeasonSelect conn seasonSelect
  where
    theSeason =
        [ Season
            (SeasonId Nothing)
            (toFields newSeason.description)
        ]
