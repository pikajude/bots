{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Bot.Connection (
  -- | In this package, a /bot/ and a /connection/ are two logically
  -- separate entities. The /bot/ is a collection of routines that react to
  -- user input; the /connection/ is the method by which user input and bot
  -- responses are communicated to each other. One /bot/ can (and probably
  -- /should/, or you don't need this library) use multiple connections.

  -- ** Connection
    Connection(..)
  , ircConnection
  , runConnection
) where

import Control.Applicative
import Control.Monad.Error
import Data.Attoparsec.ByteString
import qualified Data.ByteString as B
import Data.Monoid                   (mempty, (<>))
import Network
import Network.Bot.Backend
import Network.Bot.Backend.IRC
import Network.IRC.ByteString.Parser
import System.IO

data Connection env = forall backend. Connection
    { -- | @attoparsec@ parser. Support for other parsers may be forthcoming, but @attoparsec@'s partial input support makes it ideal for networking.
      parser :: Parser (Packet backend)
      -- | Connection backend.
    , backend :: Backend backend
      -- | The bot's \"respond loop\". Should return 'True' if the bot
      -- should continue operating, or 'False' if it should terminate this
      -- connection.
    , reactor :: Packet backend -> env Bool
      -- | Connection method.
    , connect :: env Handle
      -- | Optional bootstrapping to perform once a connection is
      -- established.
    , setup :: Handle -> env ()
    }

ircConnection :: MonadIO m => HostName -> PortID -> Connection m
ircConnection h p = Connection
    { parser = ircLine <* string "\r\n"
    , backend = ircBackend
    , reactor = \pkt -> liftIO (print pkt) >> return True
    , connect = liftIO $ connectTo h p
    , setup = const (return ())
    }

runConnection :: MonadIO m => Connection m -> m ()
runConnection (Connection p _ r c s) = do
    h <- c
    s h
    fix (\f continue leftovers -> do
        line <- liftIO $ B.hGetSome h 8192
        case continue $ leftovers <> line of
            Fail t ss err ->
                error $ "Around " ++ show t ++ ": " ++ err ++ "\nWanted: " ++ show ss
            Partial cont -> f cont mempty
            Done rest q -> r q >> f continue rest
        ) (parse p) mempty
