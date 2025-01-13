{-# LANGUAGE TemplateHaskell #-}

module F1Pools.Time (
    chicagoUTCTime,
    chicagoLocalTime,
    tzChicago,
) where

import Data.Time (LocalTime, UTCTime)
import Data.Time.Zones (TZ, localTimeToUTCTZ, utcToLocalTimeTZ)
import Data.Time.Zones.TH (includeTZFromDB)

tzChicago :: TZ
tzChicago = $(includeTZFromDB "America/Chicago")

chicagoUTCTime :: LocalTime -> UTCTime
chicagoUTCTime = localTimeToUTCTZ tzChicago

chicagoLocalTime :: UTCTime -> LocalTime
chicagoLocalTime = utcToLocalTimeTZ tzChicago
