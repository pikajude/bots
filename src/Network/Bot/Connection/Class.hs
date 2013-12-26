{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Bot.Connection.Class (
  -- | In this package, a /bot/ and a /connection/ are two logically
  -- separate entities. The /bot/ is a collection of routines that react to
  -- user input; the /connection/ is the method by which user input and bot
  -- responses are communicated to each other. One /bot/ can (and probably
  -- /should/, or you don't need this library) use multiple connections.

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

data Connection = forall backend env. MonadIO env => Connection
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
      -- | Used to execute this 'Connection' as an IO action.
    , execute :: forall a. env a -> IO a
    }

-- | Run a readloop forever.
runConnection :: Connection -> IO ()
runConnection (Connection p _ r c s eb) = eb $ do
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
