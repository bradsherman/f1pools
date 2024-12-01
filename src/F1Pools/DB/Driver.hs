{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module F1Pools.DB.Driver (
    Driver,
    DriverId,
    DriverId' (..),
    pDriverId,
    DriverIdField,
    Driver' (..),
    pDriver,
    runDriverSelect,
    driverTable,
    driverSelect,
    insertDrivers,
    deleteDriver,
    getDriver,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import Database.PostgreSQL.Simple qualified as PSQL
import GHC.Generics (Generic)
import Opaleye (
    Delete (..),
    Field,
    Insert (..),
    Select,
    SqlInt4,
    SqlText,
    Table,
    rCount,
    runDelete,
    runSelect,
    selectTable,
    table,
    tableField,
    toFields,
    where_,
    (.<=),
    (.==),
 )
import Web.FormUrlEncoded (FromForm, fromForm, parseUnique)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)

newtype DriverId' a = DriverId a
    deriving newtype (Show, Generic, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)
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
instance FromJSON Driver
instance
    ( FromHttpApiData a
    , FromHttpApiData b
    , FromHttpApiData c
    , FromHttpApiData d
    ) =>
    FromForm (Driver' a b c d)
    where
    fromForm f =
        Driver
            <$> parseUnique "driverId" f
            <*> parseUnique "firstName" f
            <*> parseUnique "lastName" f
            <*> parseUnique "team" f

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

getDriver :: DriverId -> Select DriverFieldRead
getDriver (DriverId dId) = do
    row@(Driver (DriverId theId) _ _ _) <- driverSelect
    where_ (theId .<= toFields dId)
    pure row

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

deleteDriver :: PSQL.Connection -> DriverId -> IO Int64
deleteDriver conn (DriverId dId) = runDelete conn theDelete
  where
    theDelete =
        Delete
            { dTable = driverTable
            , dWhere = \(Driver (DriverId theId) _ _ _) -> theId .== toFields dId
            , dReturning = rCount
            }
