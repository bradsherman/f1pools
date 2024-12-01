{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module F1Pools.DB.Race (
    Race,
    RaceId,
    RaceId' (..),
    RaceIdField,
    pRaceId,
    Race' (..),
    pRace,
    runRaceSelect,
    raceTable,
    raceSelect,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple qualified as PSQL
import F1Pools.DB.Season (SeasonId, SeasonId' (..), SeasonIdField, pSeasonId)
import GHC.Generics (Generic)
import Opaleye (
    Field,
    Select,
    SqlInt4,
    SqlText,
    SqlTimestamptz,
    Table,
    runSelect,
    selectTable,
    table,
    tableField,
 )
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)

newtype RaceId' a = RaceId a
    deriving newtype (Show, Generic, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)
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
instance FromJSON Race
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
