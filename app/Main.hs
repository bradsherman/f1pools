{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Database.PostgreSQL.Simple (connectPostgreSQL)
import F1Pools.API (f1poolsApp)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
    putStrLn "Running server on port 8080..."
    conn <- connectPostgreSQL "postgresql://postgres:postgres@localhost:5432/formula_one?user=postgres"
    run 8080 (f1poolsApp conn)
