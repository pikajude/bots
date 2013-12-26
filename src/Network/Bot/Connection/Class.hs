{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- | Connections.
module Network.Bot.Connection.Class (
  -- ** Connection
    Connection(..)
  , runConnection
) where

import Control.Monad.Error
import Data.Attoparsec.ByteString
import qualified Data.ByteString as B
import Data.Monoid                   (mempty, (<>))
import Network.Bot.Backend
import System.IO

-- | A connection.
data Connection = forall backend env. MonadIO env => Connection
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
runConnection :: Connection -> IO ()
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
