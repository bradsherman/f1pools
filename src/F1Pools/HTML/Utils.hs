{-# LANGUAGE OverloadedStrings #-}

module F1Pools.HTML.Utils (parseNonEmptyText) where

import Data.Text (Text)
import Web.FormUrlEncoded (Form, parseUnique)

parseNonEmptyText :: Text -> Form -> Either Text Text
parseNonEmptyText f form = do
    case parseUnique f form of
        Left e -> Left e
        Right v -> if v == "" then Left ("must pass non-empty string for " <> f) else Right v
