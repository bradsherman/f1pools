{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module F1Pools.HTML.Driver (
    DriverPage (..),
    NewDriver (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import F1Pools.DB.Driver (
    Driver,
    Driver' (..),
    DriverId,
    DriverId' (DriverId),
 )
import F1Pools.Pages (rootF1Page_)
import GHC.Generics (Generic)
import Htmx.Lucid.Core (OnEvent (DomOnEvent), hxOn_, hxPost_, hxTarget_)
import Htmx.Lucid.Extra (hxDelete_)
import Lucid (
    ToHtml,
    button_,
    class_,
    div_,
    for_,
    form_,
    h3_,
    id_,
    input_,
    label_,
    name_,
    table_,
    td_,
    th_,
    toHtml,
    toHtmlRaw,
    tr_,
    type_,
 )
import Web.FormUrlEncoded (Form, FromForm, fromForm, parseUnique)
import Web.Internal.HttpApiData (showt)

data NewDriver = NewDriver
    { ndFirstName :: !Text
    , ndLastName :: !Text
    , ndTeam :: !Text
    }
    deriving (Show, Generic)
instance ToJSON NewDriver
instance FromJSON NewDriver

parseNonEmptyText :: Text -> Form -> Either Text Text
parseNonEmptyText f form = do
    case parseUnique f form of
        Left e -> Left e
        Right v -> if v == "" then Left ("must pass non-empty string for " <> f) else Right v

instance FromForm NewDriver where
    fromForm f =
        NewDriver
            <$> parseNonEmptyText "firstName" f
            <*> parseNonEmptyText "lastName" f
            <*> parseNonEmptyText "team" f

newtype DriverPage = DriverPage
    { drivers :: [Driver]
    }
    deriving (Show, Generic, ToJSON)

instance ToHtml DriverPage where
    toHtml page = do
        rootF1Page_ . div_ $ do
            div_ [id_ "drivers-table"] . toHtml $ drivers page
            h3_ [class_ "font-bold text-2xl mt-4 mb-4"] "Add A Driver"
            form_
                [ class_ "grid grid-cols-4 gap-4"
                , hxTarget_ "#drivers-table"
                , hxOn_ (DomOnEvent ":after-request") "this.reset()"
                , hxPost_ "/drivers/new"
                ]
                $ do
                    div_ [class_ "grid flex-col"] $ do
                        label_ [for_ "firstName"] "First Name"
                        input_ [type_ "text", id_ "firstName", name_ "firstName", class_ "border rounded-md p-2"]
                    div_ [class_ "grid flex-col"] $ do
                        label_ [for_ "lastName"] "Last Name"
                        input_ [type_ "text", id_ "lastName", name_ "lastName", class_ "border rounded-md p-2"]
                    div_ [class_ "grid flex-col"] $ do
                        label_ [for_ "team"] "Team"
                        input_ [type_ "text", id_ "team", name_ "team", class_ "border rounded-md p-2"]
                    button_
                        [class_ "w-1/2 h-1/2 rounded-full self-end bg-green-400"]
                        "Add Driver"
    toHtmlRaw = toHtml

instance ToHtml DriverId where
    toHtml (DriverId dId) = toHtml $ show dId
    toHtmlRaw = toHtml

instance ToHtml Driver where
    toHtml driver =
        tr_ [class_ "border grid grid-cols-5 gap-4"] $ do
            td_ [class_ "p-2 content-center"] (toHtml driver.driverId)
            td_ [class_ "p-2 content-center"] (toHtml driver.firstName)
            td_ [class_ "p-2 content-center"] (toHtml driver.lastName)
            td_ [class_ "p-2 content-center"] (toHtml driver.team)
            td_ [class_ "p-2"] . div_ $ do
                button_ [class_ "bg-amber-300 rounded-lg p-2 mr-2"] "Edit"
                button_ [class_ "bg-red-600 rounded-lg p-2", hxTarget_ "#drivers-table", hxDelete_ ("/drivers/" <> showt driver.driverId)] "Delete"
    toHtmlRaw = toHtml

instance ToHtml [Driver] where
    toHtml seasons = do
        h3_ [class_ "font-bold text-2xl mt-4 mb-4"] "Drivers"
        table_ [class_ "table-auto mt-3 mb-3 w-full"] $ do
            tr_ [class_ "grid grid-cols-5 gap-4"] $ do
                th_ [class_ "p-2 justify-self-start"] "Driver Id"
                th_ [class_ "p-2 justify-self-start"] "First Name"
                th_ [class_ "p-2 justify-self-start"] "Last Name"
                th_ [class_ "p-2 justify-self-start"] "Team"
                th_ [class_ "p-2 justify-self-start"] ""
            foldMap toHtml seasons
    toHtmlRaw = toHtml
