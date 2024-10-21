{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module F1Pools.DB (
    -- * Season
    Season,
    SeasonId,
    SeasonId' (..),
    Season' (seasonId),
    runSeasonSelect,
    seasonTable,
    seasonSelect,

    -- * Race
    Race,
    RaceId,
    RaceId' (..),
    Race' (..),
    runRaceSelect,
    raceTable,
    raceSelect,

    -- * Race Tier
    RaceTier,
    runRaceTierSelect,
    raceTierTable,
    raceTierSelect,

    -- * Driver
    Driver,
    DriverId,
    DriverId' (..),
    Driver' (..),
    runDriverSelect,
    driverTable,
    driverSelect,
    insertDrivers,

    -- * RaceDriver
    RaceDriver,
    runRaceDriverSelect,
    raceDriverTable,
    raceDriverSelect,
) where

import Data.Aeson (ToJSON)
import Data.Int (Int64)
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple qualified as PSQL
import GHC.Generics (Generic)
import Opaleye (
    Field,
    Insert (..),
    Select,
    SqlFloat8,
    SqlInt4,
    SqlText,
    SqlTimestamptz,
    Table,
    rCount,
    runSelect,
    selectTable,
    table,
    tableField,
 )

newtype SeasonId' a = SeasonId a
    deriving (Show, Generic, ToJSON)
type SeasonId = SeasonId' Int
type SeasonIdField = SeasonId' (Field SqlInt4)
type SeasonIdFieldMaybe = SeasonId' (Maybe (Field SqlInt4))
$(makeAdaptorAndInstanceInferrable "pSeasonId" ''SeasonId')

newtype Season' a = Season {seasonId :: a}
    deriving (Show, Generic, ToJSON)
type Season = Season' SeasonId
type SeasonFieldWrite = Season' SeasonIdFieldMaybe
type SeasonFieldRead = Season' SeasonIdField
$(makeAdaptorAndInstanceInferrable "pSeason" ''Season')

seasonTable :: Table SeasonFieldWrite SeasonFieldRead
seasonTable =
    table
        "season"
        ( pSeason
            Season{seasonId = pSeasonId (SeasonId (tableField "id"))}
        )

seasonSelect :: Select SeasonFieldRead
seasonSelect = selectTable seasonTable

runSeasonSelect :: PSQL.Connection -> Select SeasonFieldRead -> IO [Season]
runSeasonSelect = runSelect

newtype RaceId' a = RaceId a
    deriving (Show, Generic, ToJSON)
type RaceId = RaceId' Int
type RaceIdField = RaceId' (Field SqlInt4)
type RaceIdFieldMaybe = RaceId' (Maybe (Field SqlInt4))
$(makeAdaptorAndInstanceInferrable "pRaceId" ''RaceId')

data Race' a b c d e = Race
    { raceId :: a
    , raceSeasonId :: b
    , raceName :: c
    , raceLocation :: d
    , startTime :: e
    }
    deriving (Show, Generic)
instance ToJSON Race
type Race = Race' RaceId SeasonId Text Text UTCTime
type RaceFieldWrite = Race' RaceIdFieldMaybe SeasonIdField (Field SqlText) (Field SqlText) (Field SqlTimestamptz)
type RaceFieldRead = Race' RaceIdField SeasonIdField (Field SqlText) (Field SqlText) (Field SqlTimestamptz)
$(makeAdaptorAndInstanceInferrable "pRace" ''Race')

raceTable :: Table RaceFieldWrite RaceFieldRead
raceTable =
    table
        "race"
        ( pRace
            Race
                { raceId = pRaceId (RaceId (tableField "id"))
                , raceSeasonId = pSeasonId (SeasonId (tableField "season_id"))
                , raceName = tableField "name"
                , raceLocation = tableField "location"
                , startTime = tableField "start_time"
                }
        )

raceSelect :: Select RaceFieldRead
raceSelect = selectTable raceTable

runRaceSelect :: PSQL.Connection -> Select RaceFieldRead -> IO [Race]
runRaceSelect = runSelect

newtype RaceTierId' a = RaceTierId a
    deriving (Show, Generic, ToJSON)
type RaceTierId = RaceTierId' Int
type RaceTierIdField = RaceTierId' (Field SqlInt4)
type RaceTierIdFieldMaybe = RaceTierId' (Maybe (Field SqlInt4))
$(makeAdaptorAndInstanceInferrable "pRaceTierId" ''RaceTierId')

data RaceTier' a b c = RaceTier
    { raceTierId :: a
    , raceId :: b
    , pointValue :: c
    }
    deriving (Show, Generic)
instance ToJSON RaceTier
type RaceTier = RaceTier' RaceTierId RaceId Double
type RaceTierFieldWrite = RaceTier' RaceTierIdFieldMaybe RaceIdField (Field SqlFloat8)
type RaceTierFieldRead = RaceTier' RaceTierIdField RaceIdField (Field SqlFloat8)
$(makeAdaptorAndInstanceInferrable "pRaceTier" ''RaceTier')

raceTierTable :: Table RaceTierFieldWrite RaceTierFieldRead
raceTierTable =
    table
        "race_tier"
        ( pRaceTier
            RaceTier
                { raceTierId = pRaceTierId (RaceTierId (tableField "id"))
                , raceId = pRaceId (RaceId (tableField "race_id"))
                , pointValue = tableField "point_value"
                }
        )

raceTierSelect :: Select RaceTierFieldRead
raceTierSelect = selectTable raceTierTable

runRaceTierSelect :: PSQL.Connection -> Select RaceTierFieldRead -> IO [RaceTier]
runRaceTierSelect = runSelect

newtype DriverId' a = DriverId a
    deriving (Show, Generic, ToJSON)
type DriverId = DriverId' Int
type DriverIdField = DriverId' (Field SqlInt4)
type DriverIdFieldMaybe = DriverId' (Maybe (Field SqlInt4))
$(makeAdaptorAndInstanceInferrable "pDriverId" ''DriverId')

data Driver' a b c d = Driver
    { driverId :: a
    , firstName :: b
    , lastName :: c
    , team :: d
    }
    deriving (Show, Generic)
instance ToJSON Driver
type Driver = Driver' DriverId Text Text Text
type DriverFieldWrite = Driver' DriverIdFieldMaybe (Field SqlText) (Field SqlText) (Field SqlText)
type DriverFieldRead = Driver' DriverIdField (Field SqlText) (Field SqlText) (Field SqlText)
$(makeAdaptorAndInstanceInferrable "pDriver" ''Driver')

driverTable :: Table DriverFieldWrite DriverFieldRead
driverTable =
    table
        "driver"
        ( pDriver
            Driver
                { driverId = pDriverId (DriverId (tableField "id"))
                , firstName = tableField "first_name"
                , lastName = tableField "last_name"
                , team = tableField "team"
                }
        )

driverSelect :: Select DriverFieldRead
driverSelect = selectTable driverTable

runDriverSelect :: PSQL.Connection -> Select DriverFieldRead -> IO [Driver]
runDriverSelect = runSelect

insertDrivers :: [DriverFieldWrite] -> Insert Int64
insertDrivers drivers =
    Insert
        { iTable = driverTable
        , iRows = drivers
        , iReturning = rCount
        , iOnConflict = Nothing
        }

data RaceDriver' a b c = RaceDriver
    { raceId :: a
    , driverId :: b
    , raceTierId :: c
    }
    deriving (Show, Generic)
instance ToJSON RaceDriver
type RaceDriver = RaceDriver' RaceId DriverId RaceTierId
type RaceDriverFieldWrite = RaceDriver' RaceIdField DriverIdField RaceTierIdField
type RaceDriverFieldRead = RaceDriver' RaceIdField DriverIdField RaceTierIdField
$(makeAdaptorAndInstanceInferrable "pRaceDriver" ''RaceDriver')

raceDriverTable :: Table RaceDriverFieldWrite RaceDriverFieldRead
raceDriverTable =
    table
        "race_driver"
        ( pRaceDriver
            RaceDriver
                { raceId = pRaceId (RaceId (tableField "race_id"))
                , driverId = pDriverId (DriverId (tableField "driver_id"))
                , raceTierId = pRaceTierId (RaceTierId (tableField "tier_id"))
                }
        )

raceDriverSelect :: Select RaceDriverFieldRead
raceDriverSelect = selectTable raceDriverTable

runRaceDriverSelect :: PSQL.Connection -> Select RaceDriverFieldRead -> IO [RaceDriver]
runRaceDriverSelect = runSelect
