{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module F1Pools.API.Driver (
    driverPage,
    handleNewDriver,
    NewDriver,
    DriverPage,
) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import Lucid (
    ToHtml,
    action_,
    button_,
    div_,
    form_,
    input_,
    method_,
    name_,
    table_,
    td_,
    th_,
    toHtml,
    toHtmlRaw,
    tr_,
    type_,
 )
import Opaleye (runInsert, toFields)
import F1Pools.DB (
    Driver,
    Driver' (..),
    DriverId,
    DriverId' (DriverId),
    driverSelect,
    insertDrivers,
    runDriverSelect,
 )
import Servant (Handler, Header, Headers, NoContent (NoContent), addHeader)
import Web.FormUrlEncoded (FromForm, fromForm, parseUnique)

data NewDriver = NewDriver
    { ndFirstName :: Text
    , ndLastName :: Text
    , ndTeam :: Text
    }
    deriving (Show)

instance FromForm NewDriver where
    fromForm f =
        NewDriver
            <$> parseUnique "firstName" f
            <*> parseUnique "lastName" f
            <*> parseUnique "team" f

handleNewDriver :: Connection -> NewDriver -> Handler (Headers '[Header "Location" String] NoContent)
handleNewDriver conn x = liftIO $ do
    void . runInsert conn $ insertDrivers theDriver
    pure $ addHeader "/drivers" NoContent
  where
    theDriver =
        [ Driver
            (DriverId Nothing)
            (toFields $ ndFirstName x)
            (toFields $ ndLastName x)
            (toFields $ ndTeam x)
        ]

newtype DriverPage = DriverPage
    { drivers :: [Driver]
    }
    deriving (Show, Generic, ToJSON)

driverPage :: Connection -> IO DriverPage
driverPage conn =
    DriverPage <$> runDriverSelect conn driverSelect

instance ToHtml DriverPage where
    toHtml page = div_ $ do
        toHtml $ drivers page
        form_ [method_ "post", action_ "/driver/new"] $ do
            input_ [type_ "text", name_ "firstName"]
            input_ [type_ "text", name_ "lastName"]
            input_ [type_ "text", name_ "team"]
            button_ "submit"
    toHtmlRaw = toHtml

instance ToHtml DriverId where
    toHtml (DriverId dId) = toHtml $ show dId
    toHtmlRaw = toHtml

instance ToHtml Driver where
    toHtml driver =
        tr_ $ do
            td_ (toHtml $ driverId driver)
            td_ (toHtml $ firstName driver)
            td_ (toHtml $ lastName driver)
            td_ (toHtml $ team driver)
    toHtmlRaw = toHtml

instance ToHtml [Driver] where
    toHtml seasons = table_ $ do
        tr_ $ do
            th_ "Driver Id"
            th_ "First Name"
            th_ "Last Name"
            th_ "Team"
        foldMap toHtml seasons
    toHtmlRaw = toHtml
