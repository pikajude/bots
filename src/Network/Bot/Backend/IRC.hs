{-# LANGUAGE TypeFamilies #-}

-- | IRC connections.
module Network.Bot.Backend.IRC (IRC, ircBackend) where

import Control.Monad.IO.Class
import Data.Monoid
import Network.Bot.Backend
import Network.IRC.ByteString.Parser

-- | Phantom type representing an IRC connection.
--
-- The 'Packet' instance for 'IRC' is 'IRCMsg'.
--
-- The 'AuthInfo' instance for 'IRC' is @()@. Use helpers from
-- Network.Bot.Backend.IRC.Extra to authenticate an IRC connection.
data IRC

type instance Packet IRC = IRCMsg
type instance AuthInfo IRC = ()

-- | A simple IRC backend without authentication support.
ircBackend :: Backend IRC
ircBackend = Backend
    { users = mempty
    , callback = \pkt -> do
        liftIO $ print pkt
        return ()
    }
