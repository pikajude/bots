{-# LANGUAGE RankNTypes #-}

-- | A connection for dAmn.
module Network.Bots.Connection.Damn (
  damnConnectionWith,
  damnConnection
) where

import Control.Monad.IO.Class
import Network
import Network.Bots.Backend.Damn
import Network.Bots.Connection.Class

-- | @damnConnection = 'damnConnectionWith' id@
damnConnection :: Connection IO
damnConnection = damnConnectionWith id

-- | Connection to dAmn.
--
-- Does not accept hostname/port since I don't think deviantART is going to
-- change their URL anytime soon.
damnConnectionWith :: MonadIO m => (forall a. m a -> IO a) -> Connection m
damnConnectionWith f = Connection
    { parser = error "damn connection parser not implemented"
    , backend = damnBackend
    , reactor = \pkt -> liftIO (print pkt) >> return True
    , connect = liftIO $ connectTo "chat.deviantart.com" (PortNumber 3900)
    , authInfo = error "Network.Bot.Backend.Damn: authInfo not set!"
    , authenticate = \_ ai -> liftIO (print ai)
    , execute = f
    }
