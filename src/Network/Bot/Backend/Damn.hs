{-# LANGUAGE TypeFamilies #-}

-- | Backend for the deviantART chat network.
module Network.Bot.Backend.Damn (
  Damn,
  DamnPacket(..),
  AuthPair(..),
  damnBackend
) where

import Data.Map (Map)
import Data.Text (Text)
import Network.Bot.Backend

-- | Phantom type representing a dAmn connection.
--
-- The 'Packet' instance for Damn is 'DamnPacket'.
--
-- The 'AuthInfo' instance for Damn is 'AuthPair'.
data Damn

-- | A packet from dAmn.
data DamnPacket = DamnPacket
                { dpCommand :: Text
                , dpParameter :: Maybe Text
                , dpArgs :: Map Text Text
                , dpBody :: Maybe Text
                } deriving Show

-- | dAmn authentication information.
data AuthPair = AuthPair
              { username :: Text
              , authtoken :: Text
              } deriving Show

type instance Packet Damn = DamnPacket
type instance AuthInfo Damn = AuthPair

-- | Backend for dAmn.
damnBackend :: Backend Damn
damnBackend = def
