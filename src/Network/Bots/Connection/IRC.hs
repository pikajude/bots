{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | A connection for IRC.
module Network.Bots.Connection.IRC (
    -- *** Building IRC connections
    ircConnection, ircConnectionWith
    -- *** Convenience re-exports
    , PortID(..)
) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.State.Concurrent
import Data.Attoparsec.ByteString
import Network
import Network.Bots.Backend.IRC
import Network.Bots.Connection.Class
import Network.IRC.ByteString.Parser

-- | IRC connection in the 'IO' monad.
--
-- @ircConnection = 'ircConnectionWith' id@
ircConnection :: HostName -> PortID -> StatefulConnection t IO
ircConnection = ircConnectionWith id

-- | @ircConnectionWith f host port@ produces an IRC connection to @host@ and @port@, using @f@
-- to execute functions from your custom monad in IO.
--
ircConnectionWith :: MonadIO m
                  => (forall a. m a -> IO a)
                  -> HostName
                  -> PortID
                  -> StatefulConnection t m
ircConnectionWith f h p tv = Connection
    { parser = ircLine <* string "\r\n"
    , backend = ircBackend
    , reactor = \pkt -> liftIO (print pkt) >> return True
    , connect = liftIO $ connectTo h p
    , authInfo = ()
    , authenticate = \_ _ -> return ()
    , execute = f . flip evalStateC tv
    }
