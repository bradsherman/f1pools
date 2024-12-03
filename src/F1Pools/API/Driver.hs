{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module F1Pools.API.Driver (
    NewDriver,
    DriverPage,

    -- * API
    DriversAPI (..),
    DriverAPI (..),
    driversHandler,
) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple (Connection)
import F1Pools.DB.Driver (
    Driver,
    Driver' (..),
    DriverId,
    DriverId' (DriverId),
    deleteDriver,
    driverSelect,
    getDriver,
    insertDrivers,
    runDriverSelect,
 )
import F1Pools.HTML.Driver (DriverPage (DriverPage), NewDriver (..))
import GHC.Generics (Generic)
import Opaleye (runInsert, toFields)
import Servant (
    Capture,
    Delete,
    FormUrlEncoded,
    Get,
    Handler,
    NamedRoutes,
    Post,
    Put,
    ReqBody,
    (:-),
    (:>),
 )
import Servant.HTML.Lucid (HTML)
import Servant.Server.Generic (AsServer)

data DriversAPI mode = DriversAPI
    { list :: mode :- Get '[HTML] DriverPage
    , create :: mode :- "new" :> ReqBody '[FormUrlEncoded] NewDriver :> Post '[HTML] [Driver]
    , driver :: mode :- Capture "driverId" DriverId :> NamedRoutes DriverAPI
    }
    deriving stock (Generic)

data DriverAPI mode = DriverAPI
    -- TODO: fix this to only be one 'Driver'
    { get :: mode :- Get '[HTML] [Driver]
    , update :: mode :- ReqBody '[FormUrlEncoded] Driver :> Put '[HTML] Driver
    , delete :: mode :- Delete '[HTML] [Driver]
    }
    deriving stock (Generic)

driversHandler :: Connection -> DriversAPI AsServer
driversHandler conn =
    DriversAPI
        { list = listDriversHandler conn
        , create = createDriverHandler conn
        , driver = driverHandler conn
        }

listDriversHandler :: Connection -> Handler DriverPage
listDriversHandler conn = liftIO (DriverPage <$> runDriverSelect conn driverSelect)

-- TODO: return only one driver
createDriverHandler :: Connection -> NewDriver -> Handler [Driver]
createDriverHandler conn newDriver = liftIO $ do
    void . runInsert conn $ insertDrivers theDriver
    runDriverSelect conn driverSelect
  where
    theDriver =
        [ Driver
            (DriverId Nothing)
            (toFields newDriver.ndFirstName)
            (toFields newDriver.ndLastName)
            (toFields newDriver.ndTeam)
        ]

driverHandler :: Connection -> DriverId -> DriverAPI AsServer
driverHandler conn driverId =
    DriverAPI
        { get = getDriverHandler conn driverId
        , update = updateDriverHandler conn driverId
        , delete = deleteDriverHandler conn driverId
        }

getDriverHandler :: Connection -> DriverId -> Handler [Driver]
getDriverHandler conn driverId =
    liftIO . runDriverSelect conn $ getDriver driverId

updateDriverHandler :: Connection -> DriverId -> Driver -> Handler Driver
updateDriverHandler _conn _driverId _updatedDriver = undefined

deleteDriverHandler :: Connection -> DriverId -> Handler [Driver]
deleteDriverHandler conn driverId = liftIO $ do
    void $ deleteDriver conn driverId
    runDriverSelect conn driverSelect
