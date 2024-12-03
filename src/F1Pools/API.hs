{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module F1Pools.API (
    f1poolsApp,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy (Proxy))
import Database.PostgreSQL.Simple (Connection)
import F1Pools.API.Driver (DriversAPI, driversHandler)
import F1Pools.API.Home (HomePage, homePage)
import F1Pools.API.Race (RacesAPI, racesHandler)
import F1Pools.API.Season (SeasonsAPI, seasonsHandler)
import F1Pools.HTML.Season ()
import GHC.Generics (Generic)
import Network.Wai (Application)
import Servant (Handler, NamedRoutes, serve)
import Servant.API (Get, (:-), (:>))
import Servant.HTML.Lucid (HTML)
import Servant.Server.Generic (AsServer)

f1poolsApp :: Connection -> Application
f1poolsApp = serve f1poolsAPI . server

f1poolsAPI :: Proxy F1PoolsAPI
f1poolsAPI = Proxy

server :: Connection -> F1PoolsAPI' AsServer
server conn =
    F1PoolsAPI'
        { seasons = seasonsHandler conn
        , drivers = driversHandler conn
        , races = racesHandler conn
        , home = homeHandler
        }

homeHandler :: Handler HomePage
homeHandler = liftIO homePage

type F1PoolsAPI = NamedRoutes F1PoolsAPI'

data F1PoolsAPI' mode = F1PoolsAPI'
    { seasons :: mode :- "seasons" :> NamedRoutes SeasonsAPI
    , drivers :: mode :- "drivers" :> NamedRoutes DriversAPI
    , races :: mode :- "races" :> NamedRoutes RacesAPI
    , home :: mode :- Get '[HTML] HomePage
    }
    deriving stock (Generic)
