{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- | Connections.
module Network.Bots.Connection.Class (
  -- ** Connection
    Connection(..)
  , runConnection

  , StatefulConnection
) where

import Control.Concurrent.STM.TVar
import Control.Monad.Error
import Control.Monad.State.Concurrent
import Data.Attoparsec.ByteString
import qualified Data.ByteString as B
import Data.Monoid                   (mempty, (<>))
import Network.Bots.Backend
import System.IO

type StatefulConnection s m = TVar s -> Connection (StateC s m)

-- | A connection.
data MonadIO env => Connection env = forall backend. Connection
    { -- | @attoparsec@ parser. Support for other parsers may be forthcoming, but @attoparsec@'s partial input support makes it ideal for networking.
      parser :: Parser (Packet backend)
      -- | Connection backend.
    , backend :: Backend backend
      -- | The bot's \"respond loop\". Should return 'True' if the bot
      -- should continue operating, or 'False' if it should terminate this
      -- connection.
    , reactor :: Packet backend -> env Bool
      -- | Connect.
    , connect :: env Handle
      -- | Authentication information.
    , authInfo :: AuthInfo backend
      -- | Self-explanatory.
    , authenticate :: Handle -> AuthInfo backend -> env ()
      -- | Used to execute this 'Connection' as an IO action.
    , execute :: forall a. env a -> IO a
    }

-- | Run a readloop forever.
runConnection :: MonadIO m => Connection m -> IO ()
runConnection (Connection p _ r c a s eb) = eb $ do
    h <- c
    s h a
    fix (\f continue leftovers -> do
        line <- liftIO $ B.hGetSome h 8192
        case continue $ leftovers <> line of
            Fail t ss err ->
                error $ "Around " ++ show t ++ ": " ++ err ++ "\nWanted: " ++ show ss
            Partial cont -> f cont mempty
            Done rest q -> r q >> f continue rest
        ) (parse p) mempty
