{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | A connection for IRC.
module Network.Bot.Connection.IRC (
    ircConnection
  , ircConnectionWith
) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Attoparsec.ByteString
import Network
import Network.Bot.Backend.IRC
import Network.Bot.Connection.Class
import Network.IRC.ByteString.Parser

-- | @ircConnectionWith f host port@ produces an IRC connection to @host@ and @port@, using @f@
-- to execute functions from your custom monad in IO.
--
-- For example, if you want to run this connection in a State monad:
--
-- @
--'runConnection' $ ircConnectionWith (\`evalState\` myInitialBotState)
--                                  \"myHost\"
--                                  ('PortNumber' 23)
-- @
ircConnectionWith :: MonadIO m => (forall a. m a -> IO a) -> HostName -> PortID -> Connection
ircConnectionWith f h p = Connection
    { parser = ircLine <* string "\r\n"
    , backend = ircBackend
    , reactor = \pkt -> liftIO (print pkt) >> return True
    , connect = liftIO $ connectTo h p
    , authInfo = ()
    , authenticate = \_ _ -> return ()
    , execute = f
    }

-- | IRC connection in the 'IO' monad.
--
-- @ircConnection = 'ircConnectionWith' id@
ircConnection :: HostName -> PortID -> Connection
ircConnection = ircConnectionWith id
