{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module F1Pools.DB.Season (
    -- * Types
    SeasonId,
    SeasonId' (..),
    pSeasonId,
    SeasonIdField,
    Season,
    Season' (..),
    pSeason,

    -- * Select
    runSeasonSelect,
    seasonTable,
    seasonSelect,

    -- * Insert
    insertSeason,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import Database.PostgreSQL.Simple qualified as PSQL
import GHC.Generics (Generic)
import Opaleye (
    Field,
    Insert (..),
    Select,
    SqlInt4,
    SqlText,
    Table,
    rCount,
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

data Season' a b = Season {seasonId :: a, seasonDescription :: b}
    deriving (Show, Generic)
type Season = Season' SeasonId Text
instance ToJSON Season
instance FromJSON Season
type SeasonFieldWrite = Season' SeasonIdFieldMaybe (Field SqlText)
type SeasonFieldRead = Season' SeasonIdField (Field SqlText)
$(makeAdaptorAndInstanceInferrable "pSeason" ''Season')

seasonTable :: Table SeasonFieldWrite SeasonFieldRead
seasonTable =
    table
        "season"
        ( pSeason
            Season
                { seasonId = pSeasonId (SeasonId (tableField "id"))
                , seasonDescription = tableField "description"
                }
        )

seasonSelect :: Select SeasonFieldRead
seasonSelect = selectTable seasonTable

runSeasonSelect :: PSQL.Connection -> Select SeasonFieldRead -> IO [Season]
runSeasonSelect = runSelect

insertSeason :: [SeasonFieldWrite] -> Insert Int64
insertSeason seasons =
    Insert
        { iTable = seasonTable
        , iRows = seasons
        , iReturning = rCount
        , iOnConflict = Nothing
        }
