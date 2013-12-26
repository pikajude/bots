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
--    myState <- newMVar someBotState
--    runBot $ stateful myState [
--      ircConnection \"irc.freenode.net\" (PortNumber 6667),
--      ircConnection \"irc.rizon.net\" (PortNumber 6667),
--      damnConnection
--    ]
-- @
module Network.Bot (
  module Network.Bot.Connection,
  module Network.Bot.Stateful
) where

import Network.Bot.Connection
import Network.Bot.Stateful
