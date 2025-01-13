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
import F1Pools.HTML.Utils (parseNonEmptyText)
import F1Pools.Pages (rootF1Page_)
import GHC.Generics (Generic)
import Htmx.Lucid.Core (OnEvent (DomOnEvent), hxOn_, hxPost_, hxTarget_)
import Htmx.Lucid.Extra (hxDelete_)
import Lucid (
    ToHtml,
    autocomplete_,
    button_,
    class_,
    dialog_,
    div_,
    for_,
    form_,
    h3_,
    id_,
    input_,
    label_,
    method_,
    name_,
    onclick_,
    p_,
    placeholder_,
    table_,
    td_,
    th_,
    toHtml,
    toHtmlRaw,
    tr_,
    type_,
    value_,
 )
import Web.FormUrlEncoded (FromForm, fromForm)
import Web.Internal.HttpApiData (showt)

data NewDriver = NewDriver
    { ndFirstName :: !Text
    , ndLastName :: !Text
    , ndTeam :: !Text
    }
    deriving (Show, Generic)
instance ToJSON NewDriver
instance FromJSON NewDriver

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
                    button_ [class_ "btn btn-secondary w-1/2 h-1/2 rounded-full self-end"] "Add Driver"
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
                button_ [class_ "btn btn-accent rounded-lg p-2 mr-2", onclick_ "editDriverModal.showModal()"] "Edit"
                button_ [class_ "btn btn-warning rounded-lg p-2", hxTarget_ "#drivers-table", hxDelete_ ("/drivers/" <> showt driver.driverId)] "Delete"
            dialog_ [id_ "editDriverModal", class_ "modal"] $
                div_ [class_ "modal-box"] $ do
                    h3_ $ toHtml ("Edit Driver: " <> driver.firstName <> " " <> driver.lastName)
                    div_ [class_ "modal-action"] $
                        form_ [method_ "dialog"] $ do
                            div_ [class_ "grid grid-cols-2 gap-4 w-full"] $ do
                                div_ [class_ "grid col-span-2"] $ do
                                    label_ [for_ "firstName"] "First Name"
                                    input_
                                        [ type_ "text"
                                        , id_ "firstName"
                                        , name_ "firstName"
                                        , placeholder_ driver.firstName
                                        , value_ driver.firstName
                                        , autocomplete_ "off"
                                        , class_ "border rounded-md p-2"
                                        ]
                                div_ [class_ "grid col-span-2"] $ do
                                    label_ [for_ "lastName"] "Last Name"
                                    input_ [type_ "text", id_ "lastName", name_ "lastName", value_ driver.lastName, class_ "border rounded-md p-2"]
                                div_ [class_ "grid col-span-2"] $ do
                                    label_ [for_ "team"] "Team"
                                    input_ [type_ "text", id_ "team", name_ "team", value_ driver.team, class_ "border rounded-md p-2"]

                                button_ [class_ "btn btn-primary"] "Update Driver"
                                button_ [class_ "btn"] "Close"
    toHtmlRaw = toHtml

instance ToHtml [Driver] where
    toHtml [] = do
        h3_ [class_ "font-bold text-2xl mt-4 mb-4"] "Drivers"
        p_ [class_ ""] "No drivers yet..."
    toHtml drivers = do
        h3_ [class_ "font-bold text-2xl mt-4 mb-4"] "Drivers"
        table_ [class_ "table-auto mt-3 mb-3 w-full"] $ do
            tr_ [class_ "grid grid-cols-5 gap-4"] $ do
                th_ [class_ "p-2 justify-self-start"] "Driver Id"
                th_ [class_ "p-2 justify-self-start"] "First Name"
                th_ [class_ "p-2 justify-self-start"] "Last Name"
                th_ [class_ "p-2 justify-self-start"] "Team"
                th_ [class_ "p-2 justify-self-start"] ""
            foldMap toHtml drivers
    toHtmlRaw = toHtml
