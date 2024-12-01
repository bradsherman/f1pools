{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module F1Pools.DB.RaceTier (
    RaceTier,
    pRaceTier,
    RaceTierId,
    RaceTierId' (..),
    RaceTierIdField,
    pRaceTierId,
    runRaceTierSelect,
    raceTierTable,
    raceTierSelect,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Database.PostgreSQL.Simple qualified as PSQL
import F1Pools.DB.Race ()
import F1Pools.DB.Race qualified as Race -- (RaceId, RaceId' (..), RaceIdField, pRaceId)
import GHC.Generics (Generic)
import Opaleye (
    Field,
    Select,
    SqlFloat8,
    SqlInt4,
    Table,
    runSelect,
    selectTable,
    table,
    tableField,
 )
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)

newtype RaceTierId' a = RaceTierId a
    deriving newtype (Show, Generic, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)
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
instance FromJSON RaceTier
type RaceTier = RaceTier' RaceTierId Race.RaceId Double
type RaceTierFieldWrite = RaceTier' RaceTierIdFieldMaybe Race.RaceIdField (Field SqlFloat8)
type RaceTierFieldRead = RaceTier' RaceTierIdField Race.RaceIdField (Field SqlFloat8)
$(makeAdaptorAndInstanceInferrable "pRaceTier" ''RaceTier')

raceTierTable :: Table RaceTierFieldWrite RaceTierFieldRead
raceTierTable =
    table
        "race_tier"
        ( pRaceTier
            RaceTier
                { raceTierId = pRaceTierId (RaceTierId (tableField "id"))
                , raceId = Race.pRaceId (Race.RaceId (tableField "race_id"))
                , pointValue = tableField "point_value"
                }
        )

raceTierSelect :: Select RaceTierFieldRead
raceTierSelect = selectTable raceTierTable

runRaceTierSelect :: PSQL.Connection -> Select RaceTierFieldRead -> IO [RaceTier]
runRaceTierSelect = runSelect
