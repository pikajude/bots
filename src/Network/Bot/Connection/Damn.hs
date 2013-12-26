{-# LANGUAGE RankNTypes #-}

-- | A connection for dAmn.
module Network.Bot.Connection.Damn (
  damnConnectionWith,
  damnConnection
) where

import Control.Monad.IO.Class
import Network
import Network.Bot.Backend.Damn
import Network.Bot.Connection.Class

-- | @damnConnection = 'damnConnectionWith' id@
damnConnection :: Connection
damnConnection = damnConnectionWith id

-- | Connection to dAmn.
--
-- Does not accept hostname/port since I don't think deviantART is going to
-- change their URL anytime soon.
damnConnectionWith :: MonadIO m => (forall a. m a -> IO a) -> Connection
damnConnectionWith f = Connection
    { parser = error "damn connection parser not implemented"
    , backend = damnBackend
    , reactor = \pkt -> liftIO (print pkt) >> return True
    , connect = liftIO $ connectTo "chat.deviantart.com" (PortNumber 3900)
    , authInfo = error "Network.Bot.Backend.Damn: authInfo not set!"
    , authenticate = \_ ai -> liftIO (print ai)
    , execute = f
    }
