{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- | Allows the same bot instance to run in parallel on multiple backends.
module Network.Bot.Backend (
  -- ** The backend
    Backend(..)
  -- *** Associated types
  , User
  , Packet
  , Privilege
  , AuthInfo

  , module Data.Default
) where

import Control.Monad.IO.Class
import Control.Monad.State           (StateT)
import Data.Default
import Data.Map                      (Map)
import Data.Monoid                   (mempty)
import Data.Text                     (Text)

-- | Generalization of a connection backend.
data Backend ty = Backend
    { -- | A list of users, associated with their channel privileges. (These are server-specific privileges, for example operator status on IRC; they are not bot-specific.)
      users :: Map Text (User ty, Privilege ty)
    -- | Backend-specific callbacks. These are intended to, for example, maintain the user list.
    , callback :: forall m. MonadIO m => Packet ty -> StateT (Backend ty) m ()
    }

instance Default (Backend a) where
    def = Backend mempty (const $ return ())

-- | User datatype.
type family User backend
-- | Privilege datatype (i.e., operator status).
type family Privilege backend
-- | Packet datatype.
type family Packet backend
-- | Authentication information.
type family AuthInfo backend
