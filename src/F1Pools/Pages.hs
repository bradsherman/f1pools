{-# LANGUAGE OverloadedStrings #-}

module F1Pools.Pages (
    rootF1Page_,
) where

import Data.Text (Text)
import Lucid (
    HtmlT,
    body_,
    charset_,
    class_,
    crossorigin_,
    doctype_,
    h1_,
    head_,
    html_,
    integrity_,
    lang_,
    meta_,
    script_,
    src_,
    title_,
    type_,
 )

rootF1Page_ :: (Monad m) => HtmlT m () -> HtmlT m ()
rootF1Page_ html = do
    doctype_
    html_ [lang_ "en"] $ do
        head_ $ do
            meta_ [charset_ "utf-8"]
            title_ "F1 Pools"
            script_
                [ src_ "https://unpkg.com/htmx.org@2.0.3"
                , integrity_ "sha384-0895/pl2MU10Hqc6jd4RvrthNlDiE9U1tWmX7WRESftEDRosgxNsQG/Ze9YMRzHq"
                , crossorigin_ "anonymous"
                , type_ "text/javascript"
                ]
                ("" :: Text)
            script_
                [src_ "https://cdn.tailwindcss.com"]
                ("" :: Text)
        body_ $ do
            h1_ [class_ "text-3l font-bold underline"] "F1 Pools"
            html
