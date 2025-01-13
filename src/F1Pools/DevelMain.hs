{-# LANGUAGE OverloadedStrings #-}

module F1Pools.DevelMain (
    update,
) where

import Control.Concurrent (MVar, ThreadId, forkFinally, killThread, newEmptyMVar, putMVar, takeMVar)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word32)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import F1Pools.API (f1poolsApp)
import Foreign.Store (
    Store (..),
    lookupStore,
    readStore,
    storeAction,
    withStore,
 )
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
    putStrLn "Running server on port 8080..."
    conn <- connectPostgreSQL "postgresql://postgres:postgres@localhost:5432/formula_one?user=postgres"
    run 8080 (f1poolsApp conn)

{- | Made for local development
 -
 - ghcid --setup=Main.update --command="stack repl lib:hyperbole" --run=DevelMain.update --warnings
 -
 - Start or restart the server.
newStore is from foreign-store.
A Store holds onto some data across ghci reloads
-}
update :: IO ()
update = do
    mtidStore <- lookupStore tidStoreNum
    case mtidStore of
        -- no server running
        Nothing -> do
            done <- storeAction doneStore newEmptyMVar
            tid <- start done
            _ <- storeAction (Store tidStoreNum) (newIORef tid)
            return ()
        -- server is already running
        Just tidStore -> do
            restartAppInNewThread tidStore
  where
    -- callCommand "xmonadctl refreshFirefox"

    doneStore :: Store (MVar ())
    doneStore = Store 0

    -- shut the server down with killThread and wait for the done signal
    restartAppInNewThread :: Store (IORef ThreadId) -> IO ()
    restartAppInNewThread tidStore = modifyStoredIORef tidStore $ \tid -> do
        killThread tid
        withStore doneStore takeMVar
        readStore doneStore >>= start

    -- \| Start the server in a separate thread.
    start ::
        MVar () ->
        -- \^ Written to when the thread is killed.
        IO ThreadId
    start done = do
        forkFinally
            main
            -- Note that this implies concurrency
            -- between shutdownApp and the next app that is starting.
            -- Normally this should be fine
            (\_ -> putMVar done ())

tidStoreNum :: Word32
tidStoreNum = 1

modifyStoredIORef :: Store (IORef a) -> (a -> IO a) -> IO ()
modifyStoredIORef store f = withStore store $ \ref -> do
    v <- readIORef ref
    f v >>= writeIORef ref
