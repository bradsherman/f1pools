{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}

module F1Pools.API.Race (
    RacesAPI (..),
    racesHandler,
) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple (Connection)
import F1Pools.DB.Race (
    Race,
    Race' (Race),
    RaceId' (RaceId),
    insertRace,
    raceSelect,
    runRaceSelect,
 )
import F1Pools.DB.Season (
    runSeasonSelect,
    seasonSelect,
 )
import F1Pools.HTML.Race (NewRace (..), RacePage (RacePage))
import GHC.Generics (Generic)
import Opaleye (runInsert, toFields)
import Servant (FormUrlEncoded, Get, Handler, Post, ReqBody, (:-), (:>))
import Servant.HTML.Lucid (HTML)
import Servant.Server.Generic (AsServer)

data RacesAPI mode = RacesAPI
    { list :: mode :- Get '[HTML] RacePage
    , create :: mode :- "new" :> ReqBody '[FormUrlEncoded] NewRace :> Post '[HTML] [Race]
    }
    deriving stock (Generic)

racesHandler :: Connection -> RacesAPI AsServer
racesHandler conn =
    RacesAPI
        { list = listRacesHandler conn
        , create = createRaceHandler conn
        }

listRacesHandler :: Connection -> Handler RacePage
listRacesHandler conn =
    RacePage
        <$> liftIO (runRaceSelect conn raceSelect)
        <*> liftIO (runSeasonSelect conn seasonSelect)

createRaceHandler :: Connection -> NewRace -> Handler [Race]
createRaceHandler conn newRace = liftIO $ do
    void . runInsert conn $ insertRace theRace
    runRaceSelect conn raceSelect
  where
    theRace =
        [ Race
            (RaceId Nothing)
            (toFields newRace.season)
            (toFields newRace.name)
            (toFields newRace.location)
            (toFields newRace.startTime)
        ]
