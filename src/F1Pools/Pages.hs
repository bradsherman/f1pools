{-# LANGUAGE OverloadedStrings #-}

module F1Pools.Pages (
    rootF1Page_,
) where

import Data.Text (Text)
import Lucid (
    HtmlT,
    a_,
    body_,
    charset_,
    class_,
    crossorigin_,
    div_,
    doctype_,
    h1_,
    head_,
    href_,
    html_,
    integrity_,
    lang_,
    li_,
    meta_,
    nav_,
    script_,
    src_,
    title_,
    type_,
    ul_,
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
            nav_ [class_ "flex-no-wrap relative flex w-full items-center justify-between bg-zinc-50 py-2 shadow-dark-mild dark:bg-neutral-700 lg:flex-wrap lg:justify-start lg:py-4"] $
                div_ [class_ "flex w-full flex-wrap items-center justify-between px-3"] $
                    div_ [class_ "!visible hidden flex-grow basis-[100%] items-center lg:!flex lg:basis-auto"] $
                        ul_ [class_ "list-style-none me-auto flex flex-col ps-0 lg:flex-row"] $ do
                            li_ [class_ "mb-4 lg:mb-0 lg:pe-2"] $
                                a_ [href_ "/", class_ "text-black/60 transition duration-200 hover:text-black/80 hover:ease-in-out focus:text-black/80 active:text-black/80 motion-reduce:transition-none dark:text-white/60 dark:hover:text-white/80 dark:focus:text-white/80 dark:active:text-white/80 lg:px-2"] "Home"
                            li_ [class_ "mb-4 lg:mb-0 lg:pe-2"] $
                                a_ [href_ "/drivers", class_ "text-black/60 transition duration-200 hover:text-black/80 hover:ease-in-out focus:text-black/80 active:text-black/80 motion-reduce:transition-none dark:text-white/60 dark:hover:text-white/80 dark:focus:text-white/80 dark:active:text-white/80 lg:px-2"] "Drivers"
                            li_ [class_ "mb-4 lg:mb-0 lg:pe-2"] $
                                a_ [href_ "/seasons", class_ "text-black/60 transition duration-200 hover:text-black/80 hover:ease-in-out focus:text-black/80 active:text-black/80 motion-reduce:transition-none dark:text-white/60 dark:hover:text-white/80 dark:focus:text-white/80 dark:active:text-white/80 lg:px-2"] "Seasons"
                            li_ [class_ "mb-4 lg:mb-0 lg:pe-2"] $
                                a_ [href_ "/races", class_ "text-black/60 transition duration-200 hover:text-black/80 hover:ease-in-out focus:text-black/80 active:text-black/80 motion-reduce:transition-none dark:text-white/60 dark:hover:text-white/80 dark:focus:text-white/80 dark:active:text-white/80 lg:px-2"] "Races"
            div_ [class_ "container mx-auto px-10 py-10"] $ do
                h1_ [class_ "text-3xl font-bold pb-4"] "F1 Pools"
                html
