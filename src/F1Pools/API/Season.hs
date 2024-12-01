{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module F1Pools.API.Season (
    SeasonsAPI (..),
    seasonsHandler,
) where

import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple (Connection)
import F1Pools.DB.Season (Season, runSeasonSelect, seasonSelect)
import GHC.Generics (Generic)
import Servant (Get, Handler, (:-))
import Servant.HTML.Lucid (HTML)
import Servant.Server.Generic (AsServer)

newtype SeasonsAPI mode = SeasonsAPI
    { list :: mode :- Get '[HTML] [Season]
    }
    deriving stock (Generic)

seasonsHandler :: Connection -> SeasonsAPI AsServer
seasonsHandler conn = SeasonsAPI{list = listSeasonsHandler conn}

listSeasonsHandler :: Connection -> Handler [Season]
listSeasonsHandler conn = liftIO (runSeasonSelect conn seasonSelect)
