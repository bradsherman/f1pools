{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module F1Pools.DB.Season (
    SeasonId,
    SeasonId' (..),
    pSeasonId,
    SeasonIdField,
    Season,
    Season' (seasonId),
    pSeason,
    runSeasonSelect,
    seasonTable,
    seasonSelect,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Database.PostgreSQL.Simple qualified as PSQL
import GHC.Generics (Generic)
import Opaleye (
    Field,
    Select,
    SqlInt4,
    Table,
    runSelect,
    selectTable,
    table,
    tableField,
 )
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)

newtype SeasonId' a = SeasonId a
    deriving newtype (Show, Generic, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)
type SeasonId = SeasonId' Int
type SeasonIdField = SeasonId' (Field SqlInt4)
type SeasonIdFieldMaybe = SeasonId' (Maybe (Field SqlInt4))
$(makeAdaptorAndInstanceInferrable "pSeasonId" ''SeasonId')

newtype Season' a = Season {seasonId :: a}
    deriving newtype (Show, Generic, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)
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
