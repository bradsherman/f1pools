{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module F1Pools.DB.RaceDriver (
    RaceDriver,
    runRaceDriverSelect,
    raceDriverTable,
    raceDriverSelect,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Database.PostgreSQL.Simple qualified as PSQL
import F1Pools.DB.Driver qualified as Driver -- (DriverId, DriverId' (DriverId), DriverIdField, pDriverId)
import F1Pools.DB.Race qualified as Race -- (RaceId, RaceId' (..), RaceIdField, pRaceId)
import F1Pools.DB.RaceTier qualified as RaceTier -- (RaceTierId, RaceTierId' (RaceTierId), RaceTierIdField, pRaceTierId)
import GHC.Generics (Generic)
import Opaleye (
    Select,
    Table,
    runSelect,
    selectTable,
    table,
    tableField,
 )

data RaceDriver' a b c = RaceDriver
    { raceId :: a
    , driverId :: b
    , raceTierId :: c
    }
    deriving (Show, Generic)
instance ToJSON RaceDriver
instance FromJSON RaceDriver
type RaceDriver = RaceDriver' Race.RaceId Driver.DriverId RaceTier.RaceTierId
type RaceDriverFieldWrite = RaceDriver' Race.RaceIdField Driver.DriverIdField RaceTier.RaceTierIdField
type RaceDriverFieldRead = RaceDriver' Race.RaceIdField Driver.DriverIdField RaceTier.RaceTierIdField
$(makeAdaptorAndInstanceInferrable "pRaceDriver" ''RaceDriver')

raceDriverTable :: Table RaceDriverFieldWrite RaceDriverFieldRead
raceDriverTable =
    table
        "race_driver"
        ( pRaceDriver
            RaceDriver
                { raceId = Race.pRaceId (Race.RaceId (tableField "race_id"))
                , driverId = Driver.pDriverId (Driver.DriverId (tableField "driver_id"))
                , raceTierId = RaceTier.pRaceTierId (RaceTier.RaceTierId (tableField "tier_id"))
                }
        )

raceDriverSelect :: Select RaceDriverFieldRead
raceDriverSelect = selectTable raceDriverTable

runRaceDriverSelect :: PSQL.Connection -> Select RaceDriverFieldRead -> IO [RaceDriver]
runRaceDriverSelect = runSelect
