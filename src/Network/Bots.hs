{-# LANGUAGE RankNTypes #-}

-- | In @bots@, a /bot/ and a /connection/ are two logically
-- separate entities. The /bot/ is a collection of routines that react to
-- user input; the /connection/ is the method by which user input and bot
-- responses are communicated to each other. One /bot/ can (and probably
-- /should/, or you don't need this library) use multiple connections.
--
-- For example, if you wanted the same bot to run simultaneously on
-- Freenode, Rizon, and the deviantART chat network, you could produce
-- something like:
--
-- @
--main :: IO ()
--main = do
--    myState <- readBotStateFromDisk
--    runBotWithState myState [
--      ircConnection \"irc.freenode.net\" (PortNumber 6667),
--      ircConnection \"irc.rizon.net\" (PortNumber 6667),
--      damnConnection
--    ]
-- @
module Network.Bots (
    module Network.Bots.Connection,
    module Network.Bots.Stateful,

    -- *** Run a bot
    runBot, runBotWithState
) where

import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import Network.Bots.Connection
import Network.Bots.Stateful

runBot :: MonadIO m => [Connection m] -> IO ()
runBot cs = foldr ((>>) . wait) (return ()) =<< go cs where
    go [] = return []
    go (c:css) = do
        a <- async $ runConnection c
        fmap (a:) (go css)

runBotWithState :: MonadIO m => a -> [StatefulConnection a m] -> IO ()
runBotWithState s cs = do
    m <- newTVarIO s
    runBot =<< mapM (statefully m) cs
